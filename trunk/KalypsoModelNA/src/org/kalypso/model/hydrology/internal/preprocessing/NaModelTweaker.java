/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.model.hydrology.internal.preprocessing;

import org.kalypso.model.hydrology.binding.model.Branching;
import org.kalypso.model.hydrology.binding.model.BranchingWithNode;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.Channel;
import org.kalypso.model.hydrology.binding.model.KontZufluss;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.binding.model.Node;
import org.kalypso.model.hydrology.binding.model.VirtualChannel;
import org.kalypso.simulation.core.SimulationException;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * Before any ascii files are written, the modell.gml (calcCase.gml) gets tweaked by this class.<br/>
 * 
 * @author Gernot Belger
 */
public class NaModelTweaker
{
  private final NaModell m_naModel;

  private final NaNodeResultProvider m_nodeResultProvider;

  public NaModelTweaker( final NaModell naModel, final NaNodeResultProvider nodeResultProvider )
  {
    m_naModel = naModel;
    m_nodeResultProvider = nodeResultProvider;
  }

  /**
   * update workspace and do some tricks in order to fix some things the fortran-kernel can not handle for now
   * 
   * @param workspace
   * @throws Exception
   */
  public void tweakModel( ) throws Exception
  {
    updateGWNet();
    updateNode2NodeNet();
    updateZuflussNet();
    updateResultAsZuflussNet();
  }

  /**
   * Updates workspace, so that interflow and channelflow dependencies gets optimized <br>
   * Groundwater flow can now run in opposite direction to channel flow.<br>
   * before: <code>
   * 
   *     -C-o (existing channel c with catchment T and downstream node o)
   *      ^
   *      T
   * 
   * </code> after: <code>
   * 
   *     -C-o 
   *        |
   *        V<T  (new virtual channel with existing catchment T, downstream node o and no upstream node)
   * 
   * </code>
   * 
   * @param workspace
   */
  private void updateGWNet( )
  {
    final IFeatureBindingCollection<Catchment> catchments = m_naModel.getCatchments();
    for( final Catchment catchment : catchments )
    {
      final Channel channel = catchment.getChannel();
      if( channel != null )
      {
        final Node node = channel.getDownstreamNode();

        /* Create new channel and relocate catchment to the new channel. */
        final Channel newChannel = m_naModel.getChannels().addNew( VirtualChannel.FEATURE_VIRTUAL_CHANNEL );
        catchment.setChannel( newChannel );

        /* Connect the new channel into the net */
        newChannel.setDownstreamNode( node );
      }
    }
  }

  /**
   * before: <br>
   * <code>
   * 
   * Node1 O <---  O Node2
   * 
   * </code> after: <br>
   * <code>
   * 
   * Node1 O <--- newVChannel <-- newNode O <-- newVChannel
   *                                      A
   *                                      |
   *                                      O-- Node2
   * 
   * </code>
   * 
   * @param workspace
   * @throws Exception
   */
  private void updateNode2NodeNet( ) throws Exception
  {
    final IFeatureBindingCollection<Node> nodes = m_naModel.getNodes();
    // Copy to array, as the list is manipulated on the fly.
    final Node[] nodeArray = nodes.toArray( new Node[nodes.size()] );
    for( final Node node : nodeArray )
    {
      final Branching branching = node.getBranching();
      if( branching != null )
      {
        if( branching instanceof BranchingWithNode )
        {
          final BranchingWithNode branchingWithNode = (BranchingWithNode) branching;

          final Node targetNode = branchingWithNode.getNode();
          if( targetNode == null )
          {
            final String message = String.format( "Node not set for branching in Node '%s'", node.getName() );
            throw new SimulationException( message );
          }

          final Node newNode = buildVChannelNet( targetNode );
          branchingWithNode.setNode( newNode );
        }
      }
    }
  }

  /**
   * put one more VChannel to each Q-source, so that this discharge will appear in the result of the connected node <br>
   * zml inflow <br>
   * before: <br>
   * <code>
   * |Channel| <- o(1) <- input.zml <br>
   * </code><br>
   * now: <br>
   * <code>
   * |Channel| <- o(1) <- |VChannel (new)| <- o(new) <- |VChannel (new)|
   *                                          A- input.zml <br>
   * </code> constant inflow <br>
   * before: <br>
   * <code>
   * |Channel| <- o(1) <- Q(constant) <br>
   * </code><br>
   * now: <br>
   * <code>
   * |Channel| <- o(1) <- |VChannel (new)| <- o(new) <- |VChannel (new)|
   *                                          A- Q(constant)<br>
   * </code>
   * 
   * @param workspace
   * @throws Exception
   */
  private void updateZuflussNet( ) throws Exception
  {
    final IFeatureBindingCollection<Node> nodes = m_naModel.getNodes();
    final Node[] nodeArray = nodes.toArray( new Node[nodes.size()] );
    for( final Node node : nodeArray )
    {
      final TimeseriesLinkType zuflussLink = node.getZuflussLink();
      if( zuflussLink != null )
      {
        final Node newNode = buildVChannelNet( node );

        // move zufluss-property to new node
        node.setZuflussLink( null );
        newNode.setZuflussLink( zuflussLink );

        final Boolean synteticZufluss = node.isSynteticZufluss();
        newNode.setIsSynteticZufluss( synteticZufluss );
      }

      final Branching branching = node.getBranching();
      if( branching instanceof KontZufluss )
      {
        // update zufluss
        final Node newNode = buildVChannelNet( node );
        // move constant-inflow to new node
        node.setBranching( null );
        newNode.setBranching( branching );
      }
    }
  }

  /**
   * if results exists (from a former simulation) for a node, use this results as input, later the upstream nodes will
   * be ignored for calculation
   * 
   * @param workspace
   * @throws Exception
   */
  private void updateResultAsZuflussNet( ) throws Exception
  {
    final IFeatureBindingCollection<Node> nodes = m_naModel.getNodes();
    final Node[] nodeArray = nodes.toArray( new Node[nodes.size()] );
    for( final Node node : nodeArray )
    {
      if( m_nodeResultProvider.checkResultExists( node ) )
      {
        final TimeseriesLinkType resultLink = node.getResultLink();
        // disconnect everything upstream (channel -> node)
        final Channel[] upstreamChannels = node.findUpstreamChannels();
        for( final Channel channel : upstreamChannels )
        {
          final Node newEndNode = nodes.addNew( Node.FEATURE_NODE );
          channel.setDownstreamNode( newEndNode );
        }

        // TODO: check: why is the resultLink not removed from 'node'?

        // add as zufluss
        final Node newNode = buildVChannelNet( node );
        newNode.setZuflussLink( resultLink );

        final Boolean isSyntetic = node.isSynteticZufluss();
        newNode.setIsSynteticZufluss( isSyntetic );
      }
    }
  }

  /**
   * before: <code>
   * 
   *     o(existing)
   * 
   * </code> after: <code>
   * 
   *  |new Channel3|
   *     |
   *     V
   *     o(new Node2)  (return value)
   *     |
   *     V
   *  |new Channel1|
   *     |
   *     V
   *     o(existing)
   * 
   * </code>
   */
  private Node buildVChannelNet( final Node existingNode ) throws Exception
  {
    final IFeatureBindingCollection<Node> nodes = m_naModel.getNodes();
    final IFeatureBindingCollection<Channel> channels = m_naModel.getChannels();

    // add to collections:
    final Channel newChannel1 = channels.addNew( VirtualChannel.FEATURE_VIRTUAL_CHANNEL );
    final Channel newChannel3 = channels.addNew( VirtualChannel.FEATURE_VIRTUAL_CHANNEL );
    final Node newNode2 = nodes.addNew( Node.FEATURE_NODE );

    /* Create network */
    newChannel3.setDownstreamNode( newNode2 );
    newNode2.setDownstreamChannel( newChannel1 );
    newChannel1.setDownstreamNode( existingNode );

    return newNode2;
  }

}
