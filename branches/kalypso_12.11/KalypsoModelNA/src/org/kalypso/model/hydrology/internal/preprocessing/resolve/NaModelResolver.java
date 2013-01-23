/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
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
package org.kalypso.model.hydrology.internal.preprocessing.resolve;

import java.util.Arrays;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.model.hydrology.binding.HydrotopeCollection;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.KontZufluss;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.binding.model.channels.Channel;
import org.kalypso.model.hydrology.binding.model.nodes.Branching;
import org.kalypso.model.hydrology.binding.model.nodes.BranchingWithNode;
import org.kalypso.model.hydrology.binding.model.nodes.INode;
import org.kalypso.model.hydrology.binding.model.nodes.Node;
import org.kalypso.model.hydrology.internal.IDManager;
import org.kalypso.model.hydrology.internal.ModelNA;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypso.model.hydrology.internal.preprocessing.NAPreprocessorException;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.ICatchmentInfos;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.ParameterHash;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * Before any ascii files are written, the NA model (modell.gml) gets tweaked by this class.
 * 
 * @author Gernot Belger
 */
public class NaModelResolver
{
  private final NaModell m_model;

  private final Node m_rootNode;

  private final ParameterHash m_landuseHash;

  private final HydrotopeCollection m_hydrotopes;

  private ICatchmentInfos m_resolvedCatchmentData;

  private final NaModelManipulator m_manipulator;

  private final IDManager m_idManager;

  public NaModelResolver( final NaModell model, final Node rootNode, final ParameterHash landuseHash, final HydrotopeCollection hydrotopes, final IDManager idManager )
  {
    m_model = model;
    m_idManager = idManager;
    m_manipulator = new NaModelManipulator( model );

    m_rootNode = rootNode;
    m_landuseHash = landuseHash;
    m_hydrotopes = hydrotopes;
  }

  public ICatchmentInfos getResolvedCatchmentData( )
  {
    return m_resolvedCatchmentData;
  }

  /**
   * update workspace and do some tricks in order to fix some things the fortran-kernel can not handle for now
   * 
   * @return Resolving the model changes the catchmen data, this method returns this updated data.
   */
  public IStatus execute( ) throws NAPreprocessorException
  {
    final IStatusCollector log = new StatusCollector( ModelNA.PLUGIN_ID );

    /*
     * 'touch' all ascii ids here, so original catchment always get the same id,
     * regardless if new catchments will be created later
     */
    final IFeatureBindingCollection<Catchment> catchments = m_model.getCatchments();
    for( final Catchment catchment : catchments )
      m_idManager.getAsciiID( catchment );

    m_resolvedCatchmentData = resolveCatchments( log );

    // TODO: branching with catchment as target

    updateGWNet();
    updateNode2NodeNet();
    updateZuflussNet();
    updateResultAsZuflussNet();

    return log.asMultiStatusOrOK( "Resolving model" );
  }

  /**
   * Resolves catchments that contain drwbm soil types. For each distinguished dwwbm soil type, a new catchment is created.
   */
  private ICatchmentInfos resolveCatchments( final IStatusCollector log ) throws NAPreprocessorException
  {
    final CatchmentResolver resolver = new CatchmentResolver( m_model, m_landuseHash, m_hydrotopes );
    final IStatus status = resolver.execute();
    if( !status.isOK() )
      log.addAll( Arrays.asList( status.getChildren() ) );

    return resolver.getResolvedCatchments();
  }

  /**
   * Updates workspace, so that interflow and channelflow dependencies gets optimized (FIXME: why 'optimized')<br>
   * Groundwater flow can now run in opposite direction to channel flow.<br>
   * This is achieved by moving every catchment to a virtual channel that is connected to the original downstream node of the original channel.
   */
  private void updateGWNet( )
  {
    final IFeatureBindingCollection<Catchment> catchments = m_model.getCatchments();
    for( final Catchment catchment : catchments )
    {
      // TODO: currently we do this with every catchment, increasing the net size considerably, but probably this is only necessary for certain situations.

      final Channel channel = catchment.getChannel();
      if( channel != null )
        m_manipulator.moveCatchmentToVirtualChannel( catchment );
    }
  }

  /**
   * FIXME: why is this needed?<br/>
   * before:
   * 
   * <pre>
   * Node1 O <---  O Node2
   * </pre>
   * 
   * after:
   * 
   * <pre>
   * Node1 O <--- newVChannel <-- newNode O <-- newVChannel
   *                                      A
   *                                      |
   *                                      O-- Node2
   * </pre>
   */
  private void updateNode2NodeNet( ) throws NAPreprocessorException
  {
    final IFeatureBindingCollection<Node> nodes = m_model.getNodes();
    // Copy to array, as the list is manipulated on the fly and wil change
    final Node[] nodeArray = nodes.toArray( new Node[nodes.size()] );
    for( final Node node : nodeArray )
    {
      final Branching branching = node.getBranching();
      if( branching instanceof BranchingWithNode )
      {
        final BranchingWithNode branchingWithNode = (BranchingWithNode)branching;

        final Node targetNode = branchingWithNode.getNode();
        if( targetNode == null )
        {
          final String message = String.format( Messages.getString( "NaModelTweaker_0" ), node.getName() ); //$NON-NLS-1$
          throw new NAPreprocessorException( message );
        }

        final Node newNode = m_manipulator.insertUpstreamVChannel( targetNode );
        branchingWithNode.setNode( newNode );
      }
    }
  }

  /**
   * put one more VChannel to each Q-source, so that this discharge will appear in the result of the connected node <br>
   * zml inflow <br/>
   * before:
   * 
   * <pre>
   * |Channel| <- o(1) <- input.zml <br>
   *              A- input.zml
   * </pre>
   * 
   * now:
   * 
   * <pre>
   * |Channel| <- o(1) <- |VChannel (new)| <- o(new) <- |VChannel (new)|
   *                                          A- input.zml
   * </pre>
   * 
   * constant inflow<br/>
   * before:
   * 
   * <pre>
   * |Channel| <- o(1) <- Q(constant) <br>
   * </pre>
   * 
   * now:
   * 
   * <pre>
   * |Channel| <- o(1) <- |VChannel (new)| <- o(new) <- |VChannel (new)|
   *                                          A- Q(constant)
   * </pre>
   */
  private void updateZuflussNet( )
  {
    final IFeatureBindingCollection<Node> nodes = m_model.getNodes();
    final Node[] nodeArray = nodes.toArray( new Node[nodes.size()] );
    for( final Node node : nodeArray )
    {
      final ZmlLink zuflussLink = node.getZuflussLink();
      if( zuflussLink.isLinkSet() )
      {
        final Node newNode = m_manipulator.insertUpstreamVChannel( node );

        // IMPORTANT: subtle: resolve value before setting the link to null, else the value will be null, beeing retreived in a lazy way.
        final TimeseriesLinkType zuflussLinkValue = zuflussLink.getTimeseriesLink();

        // move zufluss-property to new node
        node.setZuflussLink( null );
        newNode.setZuflussLink( zuflussLinkValue );

        final Boolean synteticZufluss = node.isSynteticZufluss();
        newNode.setIsSynteticZufluss( synteticZufluss );
      }

      final Branching branching = node.getBranching();
      if( branching instanceof KontZufluss )
      {
        // update zufluss
        final Node newNode = m_manipulator.insertUpstreamVChannel( node );
        // move constant-inflow to new node
        node.setBranching( null );
        newNode.setBranching( branching );
      }
    }
  }

  /**
   * if results exists (from a former simulation) for a node, use this results as input, later the upstream nodes will
   * be ignored for calculation<br/>
   * FIXME: strange order... if we added upstreams node via the other manipulation methods (e.g. branchig at this node), they will now get disconnected from the net. Is this correct?
   * 
   * @param workspace
   * @throws Exception
   */
  private void updateResultAsZuflussNet( )
  {
    final IFeatureBindingCollection<Node> nodes = m_model.getNodes();
    final Node[] nodeArray = nodes.toArray( new Node[nodes.size()] );
    for( final Node node : nodeArray )
    {
      final ZmlLink resultLink = node.getResultAsInflowLinkChecked( m_rootNode );
      if( resultLink != null )
      {
        // disconnect everything upstream (channel -> node)
        final Channel[] upstreamChannels = node.findUpstreamChannels();
        for( final Channel channel : upstreamChannels )
        {
          // FIXME: is a new ndoe for each channel necessary; why not connect to one new node?
          final Node newEndNode = nodes.addNew( INode.FEATURE_NODE );
          channel.setDownstreamNode( newEndNode );
        }

        // add as zufluss
        // FIXME: the manipulation with inflow nodes will not affect this node here, is this corect?
        final Node newNode = m_manipulator.insertUpstreamVChannel( node );
        newNode.setZuflussLink( resultLink.getTimeseriesLink() );

        final Boolean isSyntetic = node.isSynteticZufluss();
        newNode.setIsSynteticZufluss( isSyntetic );
      }
    }
  }
}