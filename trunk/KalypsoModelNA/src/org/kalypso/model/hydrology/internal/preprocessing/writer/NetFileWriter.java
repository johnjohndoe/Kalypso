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
package org.kalypso.model.hydrology.internal.preprocessing.writer;

import java.io.File;
import java.io.PrintWriter;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.StringUtils;
import org.kalypso.contribs.java.net.UrlUtilities;
import org.kalypso.contribs.java.util.FortranFormatHelper;
import org.kalypso.convert.namodel.NAConfiguration;
import org.kalypso.convert.namodel.manager.IDManager;
import org.kalypso.convert.namodel.net.NetElement;
import org.kalypso.convert.namodel.net.visitors.CompleteDownstreamNetAsciiWriterVisitor;
import org.kalypso.convert.namodel.net.visitors.RootNodeCollector;
import org.kalypso.convert.namodel.net.visitors.SimulationVisitor;
import org.kalypso.convert.namodel.net.visitors.WriteAsciiVisitor;
import org.kalypso.convert.namodel.timeseries.NAZMLGenerator;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.model.hydrology.binding.NAControl;
import org.kalypso.model.hydrology.binding.model.Branching;
import org.kalypso.model.hydrology.binding.model.BranchingWithNode;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.Channel;
import org.kalypso.model.hydrology.binding.model.KontEntnahme;
import org.kalypso.model.hydrology.binding.model.KontZufluss;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.binding.model.Node;
import org.kalypso.model.hydrology.binding.model.StorageChannel;
import org.kalypso.model.hydrology.binding.model.Ueberlauf;
import org.kalypso.model.hydrology.binding.model.Verzweigung;
import org.kalypso.model.hydrology.internal.preprocessing.RelevantNetElements;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITupleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.ogc.sensor.zml.ZmlURL;
import org.kalypso.simulation.core.SimulationException;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

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
public class NetFileWriter extends AbstractCoreFileWriter
{
  private static final class ZuflussBean
  {
    public final StringBuffer m_specialBuffer = new StringBuffer();

    public final int m_izug;

    public final int m_iabg;

    public final int m_iueb;

    public final int m_izuf;

    public final int m_ivzwg;

    public final Number m_value;

    public final Node m_branchNode;

    public ZuflussBean( final int izug, final int iabg, final int iueb, final int izuf, final int ivzwg, final double value, final Node branchNode )
    {
      m_izug = izug;
      m_iabg = iabg;
      m_iueb = iueb;
      m_izuf = izuf;
      m_ivzwg = ivzwg;
      m_value = value;
      m_branchNode = branchNode;
    }
  }

  private final UrlUtilities m_urlUtilities = new UrlUtilities();

  private final RelevantNetElements m_relevantElements = new RelevantNetElements();

  private final NAConfiguration m_conf;

  private final Node m_rootNode;

  private final TimeseriesFileManager m_tsFileManager;

  private final GMLWorkspace m_modelWorkspace;

  private final GMLWorkspace m_synthNWorkspace;

  private final IDManager m_idManager;

  public NetFileWriter( final NAConfiguration conf, final Node rootNode, final Logger logger, final GMLWorkspace modelWorkspace, final GMLWorkspace synthNWorkspace )
  {
    super( logger );

    m_conf = conf;
    m_rootNode = rootNode;
    m_modelWorkspace = modelWorkspace;
    m_synthNWorkspace = synthNWorkspace;

    final NAControl metaControl = m_conf.getMetaControl();
    final boolean usePrecipitationForm = metaControl.isUsePrecipitationForm();
    m_idManager = m_conf.getIdManager();
    m_tsFileManager = new TimeseriesFileManager( m_idManager, usePrecipitationForm );
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

    final NaModell naModel = (NaModell) m_modelWorkspace.getRootFeature();
    final IFeatureBindingCollection<Channel> channels = naModel.getChannels();

    // REMARK: Fix: using LinkedHashMap, so net generation is independent of current gml-id of channel.
    // Else, the net was generated differently for every simulation (due to the fact that the gml-ids change....)
    final Map<String, NetElement> netElements = new LinkedHashMap<String, NetElement>();
    // generate net elements, each channel represents a netelement
    for( final Channel channelFE : channels )
    {
      final NetElement netElement = new NetElement( m_modelWorkspace, m_synthNWorkspace, channelFE, m_conf, getLogger() );
      netElements.put( channelFE.getId(), netElement );
    }

    // find dependencies: node - node
    final IFeatureBindingCollection<Node> nodes = naModel.getNodes();
    for( final Node upStreamNode : nodes )
    {
      final Channel upStreamChannel = upStreamNode.getDownstreamChannel();
      final Node[] relatedNode = findRelatedNodes( upStreamNode );
      for( final Node downStreamNode : relatedNode )
      {
        final Channel downStreamChannelFE = downStreamNode.getDownstreamChannel();

        if( upStreamChannel == null )
        {
          final String message = String.format( "Inconsistent net: Node '%s' with branch has no upstream channel.", upStreamNode.getName() );
          throw new SimulationException( message );
        }

        if( downStreamChannelFE == null )
        {
          final String message = String.format( "Inconsistent net: Node '%s' with branch has no downstream channel.", upStreamNode.getName() );
          throw new SimulationException( message );
        }

        if( upStreamChannel == downStreamChannelFE )
        {
          logWarning( "Impossible net at %s: Node-Node relation to itself", upStreamChannel );
          // FIXME: shouldn't we throw an exception here?
          continue;
        }

        // set dependency
        final NetElement upStreamElement = netElements.get( upStreamChannel.getId() );
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
    final IFeatureBindingCollection<Catchment> catchments = naModel.getCatchments();
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
      final IRelationType rt1 = (IRelationType) catchment.getFeatureType().getProperty( NaModelConstants.GRUNDWASSERABFLUSS_MEMBER );
      final Feature[] abflussFEs = m_modelWorkspace.resolveLinks( catchment, rt1 );
      for( final Feature abflussFE : abflussFEs )
      {
        final IRelationType rt2 = (IRelationType) abflussFE.getFeatureType().getProperty( NaModelConstants.CATCHMENT_PROP_NGWZU );
        final Catchment downStreamCatchmentFE = (Catchment) m_modelWorkspace.resolveLink( abflussFE, rt2 );
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
    getLogger().log( Level.WARNING, msg );
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

  @Override
  protected void writeContent( final PrintWriter netBuffer ) throws SimulationException, Exception
  {
    // FIXME: this method should only collect the relevant elements; files should be written spearately
    final NetElement[] netElements = generateNetElements();

    // collect netelements that are direct upstream of result nodes
    final RootNodeCollector rootNodeVisitor = new RootNodeCollector( netElements, m_rootNode );
    final NetElement[] rootNetElements = rootNodeVisitor.getRootNodeElements();

    // write asciifiles: upstream-network of root nodes
    final WriteAsciiVisitor writeAsciiVisitor = new WriteAsciiVisitor( m_relevantElements, netBuffer, m_tsFileManager );
    final SimulationVisitor simulationVisitor = new SimulationVisitor( writeAsciiVisitor );
    for( final NetElement element : rootNetElements )
      simulationVisitor.visit( element );

    // write ascii: complete network below root nodes
    final CompleteDownstreamNetAsciiWriterVisitor completeNetVisitor = new CompleteDownstreamNetAsciiWriterVisitor( m_relevantElements, netBuffer );
    for( final NetElement netElement : netElements )
      netElement.accept( completeNetVisitor );

    final Node[] nodeCollector = m_relevantElements.getNodes();
    netBuffer.append( "99999\n" ); //$NON-NLS-1$
    appendNodeList( nodeCollector, netBuffer );
    netBuffer.append( "99999\n" ); //$NON-NLS-1$
  }

  public void appendNodeList( final Node[] nodes, final PrintWriter netBuffer ) throws Exception, Exception
  {
    // FIXME: theses nodes do not contain the branching nodes

    final Map<Node, ZuflussBean> nodeInfos = new LinkedHashMap<Node, ZuflussBean>();

    /* First collect info and potentially add branching nodes */
    for( final Node node : nodes )
    {
      // QQ have the priority over "Verzweigung", and all of them are mutually exclusive so this is safe
      final ZuflussBean qqRelation = getQQRelationZuflussBean( node );
      final ZuflussBean zuflussBean = qqRelation != null ? qqRelation : appendZuflussStuff( node );
      nodeInfos.put( node, zuflussBean );

      if( zuflussBean.m_branchNode != null )
      {
        /* Do not overwrite existing info */
        if( !nodeInfos.containsKey( zuflussBean.m_branchNode ) )
          nodeInfos.put( zuflussBean.m_branchNode, new ZuflussBean( 0, 0, 0, 0, 0, Double.NaN, null ) );
      }
    }

    /* Write thes infos to file */
    final Set<Entry<Node, ZuflussBean>> entrySet = nodeInfos.entrySet();
    for( final Entry<Node, ZuflussBean> entry : entrySet )
    {
      final Node node = entry.getKey();
      final ZuflussBean zuflussBean = entry.getValue();

      final int nodeID = m_idManager.getAsciiID( node );

      netBuffer.append( FortranFormatHelper.printf( nodeID, "i5" ) ); //$NON-NLS-1$

      netBuffer.append( String.format( "%5d", zuflussBean.m_izug ) ); //$NON-NLS-1$
      netBuffer.append( String.format( "%5d", zuflussBean.m_iabg ) ); //$NON-NLS-1$
      netBuffer.append( String.format( "%5d", zuflussBean.m_iueb ) ); //$NON-NLS-1$
      netBuffer.append( String.format( "%5d", zuflussBean.m_izuf ) ); //$NON-NLS-1$
      netBuffer.append( String.format( "%5d", zuflussBean.m_ivzwg ) + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$
      netBuffer.append( zuflussBean.m_specialBuffer.toString() );

      /* TODO: we should also consider the additional nodes by QQ rleations; but as QQ rleations do not work..... */
    }

    // ENDKNOTEN
    netBuffer.append( " 9001    0    0    0    0    0\n" ); //$NON-NLS-1$
    netBuffer.append( "10000    0    0    0    0    0\n" ); //$NON-NLS-1$
  }

  private ZuflussBean appendZuflussStuff( final Node node ) throws Exception
  {
    // TODO: like this, only one branching can be used at the same time. Also, braching and zufluss cannot appear at
    // the same time. Is both intented? Isn't kalypso-na able to do more?
    final Branching branching = node.getBranching();
    if( branching != null )
      return appendBranching( branching );

    final TimeseriesLinkType zuflussLink = node.getZuflussLink();
    if( zuflussLink != null )
      return appendZuflussLink( node, zuflussLink );

    return new ZuflussBean( 0, 0, 0, 0, 0, Double.NaN, null );
  }

  private ZuflussBean appendZuflussLink( final Node node, final TimeseriesLinkType zuflussLink ) throws Exception
  {
    final ZuflussBean bean = new ZuflussBean( 0, 0, 0, 5, 0, Double.NaN, null );

    // FIXME: awful: this code does too much at once!
    final String zuflussFileName = getZuflussEingabeDateiString( node, m_conf );
    final File targetFile = new File( m_conf.getAsciiBaseDir(), "zufluss/" + zuflussFileName ); //$NON-NLS-1$
    final File parent = targetFile.getParentFile();
    if( !parent.exists() )
      parent.mkdirs();
    final String zuflussFile = ZmlURL.getIdentifierPart( zuflussLink.getHref() );
    final URL linkURL = m_urlUtilities.resolveURL( m_modelWorkspace.getContext(), zuflussFile );
    if( !targetFile.exists() )
    {
      final StringBuffer writer = new StringBuffer();
      final IObservation observation = ZmlFactory.parseXML( linkURL ); //$NON-NLS-1$

      final Boolean isSynteticZufluss = node.isSynteticZufluss();

      if( isSynteticZufluss != null && isSynteticZufluss )
      {
        final NAControl metaControl = m_conf.getMetaControl();
        final Integer minutesOfTimestep = metaControl.getMinutesOfTimestep();

        if( metaControl.isUsePrecipitationForm() )
        {
          final ITupleModel values = observation.getValues( null );
          final IAxis[] axis = observation.getAxisList();
          final IAxis dateAxis = ObservationUtilities.findAxisByType( axis, ITimeseriesConstants.TYPE_DATE );
          final long simulationStartDateMillis = ((Date) values.get( 0, dateAxis )).getTime();
          final long simulationEndDateMillis = ((Date) values.get( values.size() - 1, dateAxis )).getTime();
          final Date simulationStartDate = new Date( 100, 0, 1 );
          final Date simulationEndDate = new Date( simulationStartDate.getTime() + simulationEndDateMillis - simulationStartDateMillis );

          NAZMLGenerator.createSyntheticFile( writer, ITimeseriesConstants.TYPE_RUNOFF, observation, simulationStartDate, simulationEndDate, minutesOfTimestep );
        }
        else
        {
          final Date simulationStart = metaControl.getSimulationStart();
          final Date simulationEnd = metaControl.getSimulationEnd();
          NAZMLGenerator.createSyntheticFile( writer, ITimeseriesConstants.TYPE_RUNOFF, observation, simulationStart, simulationEnd, minutesOfTimestep );
        }
      }
      else
        NAZMLGenerator.createFile( writer, ITimeseriesConstants.TYPE_RUNOFF, observation );

      FileUtils.writeStringToFile( targetFile, writer.toString() );
    }
    bean.m_specialBuffer.append( "    1234\n" ); // dummyLine //$NON-NLS-1$
    bean.m_specialBuffer.append( ".." + File.separator + "zufluss" + File.separator + zuflussFileName + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    return bean;
  }

  private ZuflussBean appendBranching( final Branching branching )
  {
    final ZuflussBean bean = getZuflussBean( branching );
    if( bean.m_value instanceof Double )
      bean.m_specialBuffer.append( String.format( Locale.US, "%10.3f", bean.m_value ) ); //$NON-NLS-1$
    else if( bean.m_value instanceof Integer )
      bean.m_specialBuffer.append( String.format( "%4d", bean.m_value ) ); //$NON-NLS-1$
    // else throw an exception...

    if( bean.m_branchNode != null )
    {
      final int branchNodeID = m_idManager.getAsciiID( bean.m_branchNode );
      bean.m_specialBuffer.append( FortranFormatHelper.printf( branchNodeID, "i8" ) + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$
    }

    return bean;
  }

  private ZuflussBean getZuflussBean( final Branching branching )
  {
    final Node branchNode;
    if( branching instanceof BranchingWithNode )
      branchNode = ((BranchingWithNode) branching).getNode();
    else
      branchNode = null;

    if( branching instanceof KontZufluss )
    {
      final double qzug = FeatureHelper.getAsDouble( branching, NaModelConstants.NODE_VERZW_QZUG_PROP, 0d );
      return new ZuflussBean( 1, 0, 0, 0, 0, qzug, branchNode );
    }

    if( branching instanceof Verzweigung )
    {
      final double zproz = FeatureHelper.getAsDouble( branching, NaModelConstants.NODE_VERZW_ZPROZ_PROP, 0d );
      return new ZuflussBean( 0, 0, 0, 0, 1, zproz, branchNode );
    }

    if( branching instanceof KontEntnahme )
    {
      final double qabg = FeatureHelper.getAsDouble( branching, NaModelConstants.NODE_VERZW_QABG_PROP, 0d );
      return new ZuflussBean( 0, 1, 0, 0, 0, qabg, branchNode );
    }

    if( branching instanceof Ueberlauf )
    {
      final double queb = FeatureHelper.getAsDouble( branching, NaModelConstants.NODE_VERZW_QUEB_PROP, 0d );
      return new ZuflussBean( 0, 0, 1, 0, 0, queb, branchNode );
    }

    throw new IllegalArgumentException( "Illegal branching type: " + branching.getClass() );
  }

  private ZuflussBean getQQRelationZuflussBean( final Node node ) throws SensorException
  {
    final Node relatedNode = node.getQQRelatedNode();
    if( relatedNode == null )
      return null;
    final IObservation observation = (IObservation) node.getProperty( NaModelConstants.NODE_QQRELATION_PROP );
    if( observation == null )
      return null;
    final StringBuffer buffer = new StringBuffer();
    final IAxis[] axisList = observation.getAxisList();
    final IAxis q1Axis = ObservationUtilities.findAxisByType( axisList, ITimeseriesConstants.TYPE_RUNOFF );
    final IAxis q2Axis = ObservationUtilities.findAxisByType( axisList, ITimeseriesConstants.TYPE_RUNOFF_RHB );
    final ITupleModel values = observation.getValues( null );
    final int count = values.size();
    if( count < 1 )
      return null;
    buffer.append( String.format( "%4d %6s\n", count, m_idManager.getAsciiID( relatedNode ) ) ); //$NON-NLS-1$
    for( int row = 0; row < count; row++ )
    {
      final double q1 = (Double) values.get( row, q1Axis );
      final double q2 = (Double) values.get( row, q2Axis );
      buffer.append( String.format( Locale.ENGLISH, "%8.3f %8.3f\n", q1, q2 ) ); //$NON-NLS-1$
    }
    final ZuflussBean bean = new ZuflussBean( 0, 0, 0, 0, 2, count, relatedNode );
    bean.m_specialBuffer.append( buffer );
    return bean;
  }

  private String getZuflussEingabeDateiString( final Feature nodeFE, final NAConfiguration conf )
  {
    final int asciiID = conf.getIdManager().getAsciiID( nodeFE );
    return "Z_" + Integer.toString( asciiID ).trim() + ".zufluss"; //$NON-NLS-1$ //$NON-NLS-2$
  }

  public TimeseriesFileManager getTsFileManager( )
  {
    return m_tsFileManager;
  }

  public RelevantNetElements getRelevantElements( )
  {
    return m_relevantElements;
  }
}