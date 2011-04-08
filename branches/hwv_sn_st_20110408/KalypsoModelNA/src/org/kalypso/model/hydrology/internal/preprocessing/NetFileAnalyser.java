/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.model.hydrology.internal.preprocessing;

import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.apache.commons.lang.StringUtils;
import org.kalypso.model.hydrology.binding.model.Branching;
import org.kalypso.model.hydrology.binding.model.BranchingWithNode;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.Channel;
import org.kalypso.model.hydrology.binding.model.Grundwasserabfluss;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.binding.model.Node;
import org.kalypso.model.hydrology.binding.model.StorageChannel;
import org.kalypso.model.hydrology.internal.IDManager;
import org.kalypso.model.hydrology.internal.preprocessing.net.NetElement;
import org.kalypso.model.hydrology.internal.preprocessing.net.visitors.CompleteDownstreamNetAsciiWriterVisitor;
import org.kalypso.model.hydrology.internal.preprocessing.net.visitors.RootNodeCollector;
import org.kalypso.model.hydrology.internal.preprocessing.net.visitors.SimulationVisitor;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * erstellt die netzdatei aus namodell.gml oder liest aus netzdatei und erstellt namodell.gml ----<br/>
 * logik bei der netzerstellung: wird im control ein rootnode angegeben, so wird das netz hierfuer erstellt. vorhandene
 * ergebnisse oberhalb des rootnode werden als zufluss gerechnet. oder wird im control kein rotnode angegeben so wird
 * das netz fuer die zu generierenden ergebnisse erstellt.<br/>
 * vorhandene ergebnisse oberhalb der zu berechneneden knoten werden als zufluss gerechnet. Wobei zu generierende
 * Ergebnisse stets neu berechnet werden und niemals als Zufluss dienen.
 * 
 * @author doemming
 */
public class NetFileAnalyser
{
  private final Node m_rootNode;

  private final Logger m_logger;

  private final IDManager m_idManager;

  private final NaModell m_model;

  public NetFileAnalyser( final Node rootNode, final Logger logger, final NaModell model, final IDManager idManager )
  {
    m_rootNode = rootNode;
    m_logger = logger;
    m_model = model;
    m_idManager = idManager;
  }

  /**
   * Generate NetElements for rrm model: also defines the relation between channels (upstream/downstream). This defines
   * in what order the net is written later.<br/>
   * IMPORTANT: if any relation in the gml-model is changed/added, this methods needs to be updated as well.
   * 
   * @param workspace
   *          the rrm workspace
   * @param synthNWorkspace
   *          the synth precipitation workspace
   * @return a HashMap containing Channel-FeatureID (key) and NetElements (value)
   */
  private NetElement[] generateNetElements( ) throws SimulationException
  {
    // x -> rootNode
    // |
    // O -> virtueller Strang generiert NR xxx
    // |
    // x -> virtueller knoten generiert NR 10000

    final IFeatureBindingCollection<Channel> channels = m_model.getChannels();

    // REMARK: Fix: using LinkedHashMap, so net generation is independent of current gml-id of channel.
    // Else, the net was generated differently for every simulation (due to the fact that the gml-ids change....)
    final Map<String, NetElement> netElements = new LinkedHashMap<String, NetElement>();
    // generate net elements, each channel represents a netelement
    for( final Channel channelFE : channels )
    {
      final NetElement netElement = new NetElement( channelFE, m_idManager );
      netElements.put( channelFE.getId(), netElement );
    }

    // find dependencies: node - node
    final IFeatureBindingCollection<Node> nodes = m_model.getNodes();
    for( final Node upStreamNode : nodes )
    {
      final Channel upstreamNodeChannel = upStreamNode.getDownstreamChannel();
      final Node[] relatedNode = findRelatedNodes( upStreamNode );
      for( final Node downStreamNode : relatedNode )
      {
        final Channel downStreamChannelFE = downStreamNode.getDownstreamChannel();

        if( upstreamNodeChannel == null )
        {
          final String message = String.format( "Inconsistent net: Node '%s' with branch has no upstream channel.", upStreamNode.getName() );
          throw new SimulationException( message );
        }

        if( downStreamChannelFE == null )
        {
          final String message = String.format( "Inconsistent net: Node '%s' with branch has no downstream channel.", upStreamNode.getName() );
          throw new SimulationException( message );
        }

        if( upstreamNodeChannel == downStreamChannelFE )
        {
          logWarning( "Impossible net at %s: Node-Node relation to itself", upstreamNodeChannel );
          // FIXME: shouldn't we throw an exception here?
          continue;
        }

        // set dependency
        final NetElement upStreamElement = netElements.get( upstreamNodeChannel.getId() );
        final NetElement downStreamElement = netElements.get( downStreamChannelFE.getId() );
        downStreamElement.addUpStream( upStreamElement );
      }
    }

    // dependency: channel - node
    for( final Channel channel : channels )
    {
      final Node downStreamNode = channel.getDownstreamNode();
      if( downStreamNode == null )
      {
        logWarning( "%s is outside network", channel );
        continue;
      }

      final Channel downStreamChannel = downStreamNode.getDownstreamChannel();
      if( downStreamChannel == null )
      {
        logWarning( "%s has no downstream connection", downStreamNode );
        continue;
      }
      // set dependency
      if( channel == downStreamChannel )
      {
        logWarning( "Impossible net at %s: channel discharges to itself", channel );
        continue;
      }

      final NetElement upStreamElement = netElements.get( channel.getId() );
      final NetElement downStreamElement = netElements.get( downStreamChannel.getId() );

      downStreamElement.addUpStream( upStreamElement );
    }

    /* storage channel -> overflow node */
    for( final Channel channel : channels )
    {
      if( channel instanceof StorageChannel )
      {
        final Node overflowNode = ((StorageChannel) channel).getOverflowNode();
        if( overflowNode != null )
        {
          final Channel downstreamChannel = overflowNode.getDownstreamChannel();
          final NetElement channelElement = netElements.get( channel.getId() );
          // Hmmm: tricky, what to do, if the node has no downstream channel?
          if( downstreamChannel != null )
          {
            final NetElement downstreamElement = netElements.get( downstreamChannel.getId() );
            downstreamElement.addUpStream( channelElement );
          }
        }
      }
    }

    // dependency: catchment -> catchment
    final IFeatureBindingCollection<Catchment> catchments = m_model.getCatchments();
    for( final Catchment catchment : catchments )
    {
      // upstream
      final Channel upstreamChannel = catchment.getChannel();
      if( upstreamChannel == null )
      {
        logWarning( "%s is not connected to network", catchment );
        continue;
      }

      final NetElement upStreamElement = netElements.get( upstreamChannel.getId() );

      final Node overflowNode = catchment.getOverflowNode();
      if( overflowNode != null )
      {
        /* If we have an overflow node, we need to set the current catchment as upstream of its downstream channel */
        final Channel overflowChannel = overflowNode.getDownstreamChannel();
        if( overflowChannel != null )
        {
          final NetElement overflowElement = netElements.get( overflowChannel.getId() );
          overflowElement.addUpStream( upStreamElement );
        }

        upStreamElement.setOverflowNode( overflowNode );
      }

      // downstream
      final Grundwasserabfluss[] abflussFEs = catchment.getGrundwasserAbflussFeatures();
      for( final Grundwasserabfluss abflussFE : abflussFEs )
      {
        final Catchment downStreamCatchmentFE = abflussFE.getNgwzu();
        if( downStreamCatchmentFE == null )
        {
          logWarning( "Downstream catchment for %s cannot be resolved.", abflussFE );
          continue;
        }
        final Channel downStreamChannelFE = downStreamCatchmentFE.getChannel();
        if( downStreamChannelFE == null )
        {
          logWarning( "%s is not connected to network.", downStreamCatchmentFE );
          continue;
        }

        final NetElement downStreamElement = netElements.get( downStreamChannelFE.getId() );
        if( downStreamElement == null )
        {
          logWarning( "%s has no downstream net element.", downStreamCatchmentFE );
          continue;
        }

        if( upstreamChannel == downStreamChannelFE )
        {
          // two catchments discharges to the same channel, no need to generate
          // dependency cause it is the same channel
          continue;
        }
        downStreamElement.addUpStream( upStreamElement );
      }
    }

    final Collection<NetElement> values = netElements.values();
    return values.toArray( new NetElement[values.size()] );
  }

  private Node[] findRelatedNodes( final Node upStreamNode )
  {
    final Collection<Node> relatedNodes = new ArrayList<Node>( 2 );

    final Branching branching = upStreamNode.getBranching();
    if( branching != null )
    {
      if( branching instanceof BranchingWithNode )
      {
        final Node branchRelatedNode = ((BranchingWithNode) branching).getNode();
        if( branchRelatedNode != null )
          relatedNodes.add( branchRelatedNode );
      }
    }

    final Node qqRelatedNode = upStreamNode.getQQRelatedNode();
    if( qqRelatedNode != null )
      relatedNodes.add( qqRelatedNode );

    return relatedNodes.toArray( new Node[relatedNodes.size()] );
  }

  private void logWarning( final String format, final Feature... netElements )
  {
    final String[] logLabels = new String[netElements.length];
    for( int i = 0; i < logLabels.length; i++ )
      logLabels[i] = getLogLabel( netElements[i] );

    final String msg = String.format( format, (Object[]) logLabels ); //$NON-NLS-1$ //$NON-NLS-2$
    m_logger.log( Level.WARNING, msg );
  }

  /**
   * TODO: not a good place, should be moved elsewhere.
   */
  private static String getLogLabel( final Feature netElement )
  {
    final String defaultName = String.format( "<Unbekannt> (GML-ID: %s)", netElement.getId() );
    final String gmlName = netElement.getName();
    final String name = StringUtils.isBlank( gmlName ) ? defaultName : gmlName;

    if( netElement instanceof Catchment )
      return String.format( "Teilgebiet #%s", name );

    if( netElement instanceof Channel )
      return String.format( "Strang #%s", name );

    if( netElement instanceof Node )
      return String.format( "Knoten #%s", name );

    return name;
  }

  public RelevantNetElements analyseNet( ) throws SimulationException, Exception
  {
    final RelevantNetElements relevantElements = new RelevantNetElements();

    // FIXME: this method should only collect the relevant elements; files should be written spearately
    final NetElement[] netElements = generateNetElements();

    // collect netelements that are direct upstream of result nodes
    final RootNodeCollector rootNodeVisitor = new RootNodeCollector( netElements, m_rootNode );
    final NetElement[] rootNetElements = rootNodeVisitor.getRootNodeElements();

    // collect upstream-network of root nodes
    final SimulationVisitor simulationVisitor = new SimulationVisitor( relevantElements );
    for( final NetElement element : rootNetElements )
      simulationVisitor.visit( element );

    // collect netelements: complete network below root nodes
    final CompleteDownstreamNetAsciiWriterVisitor completeNetVisitor = new CompleteDownstreamNetAsciiWriterVisitor( relevantElements );
    for( final NetElement netElement : netElements )
      completeNetVisitor.visit( netElement );

    return relevantElements;
  }
}