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
package org.kalypso.convert.namodel.manager;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.apache.commons.io.IOUtils;
import org.kalypso.contribs.java.net.UrlUtilities;
import org.kalypso.contribs.java.util.FortranFormatHelper;
import org.kalypso.convert.namodel.NAConfiguration;
import org.kalypso.convert.namodel.NaModelConstants;
import org.kalypso.convert.namodel.i18n.Messages;
import org.kalypso.convert.namodel.net.NetElement;
import org.kalypso.convert.namodel.net.visitors.CompleteDownstreamNetAsciiWriterVisitor;
import org.kalypso.convert.namodel.net.visitors.RootNodeCollectorVisitor;
import org.kalypso.convert.namodel.net.visitors.SimulationVisitor;
import org.kalypso.convert.namodel.net.visitors.WriteAsciiVisitor;
import org.kalypso.convert.namodel.timeseries.NAZMLGenerator;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.ogc.sensor.zml.ZmlURL;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * @author doemming erstellt die netzdatei aus namodell.gml oder liest aus netzdatei und erstellt namodell.gml ----
 *         logik bei der netzerstellung: wird im control ein rootnode angegeben, so wird das netz hierfuer erstellt.
 *         vorhandene ergebnisse oberhalb des rootnode werden als zufluss gerechnet. oder wird im control kein rotnode
 *         angegeben so wird das netz fuer die zu generierenden ergebnisse erstellt vorhandene ergebnisse oberhalb der
 *         zu berechneneden knoten werden als zufluss gerechnet. Wobei zu generierende Ergebnisse stets neu berechnet
 *         werden und niemals als Zufluss dienen.
 */
public class NetFileManager extends AbstractManager
{
  public static boolean DEBUG = false;

  final public NAConfiguration m_conf;

  private final UrlUtilities m_urlUtilities;

  // private IPropertyType m_property;

  public NetFileManager( NAConfiguration conf ) throws IOException
  {
    super( conf.getNetFormatURL() );
    m_conf = conf;
    m_urlUtilities = new UrlUtilities();
  }

  @Override
  public String mapID( int id, IFeatureType ft )
  {
    return ft.getQName().getLocalPart() + id;
  }

  /**
   * importing ascii file to gml-modell
   * 
   * @see org.kalypso.convert.namodel.manager.AbstractManager#parseFile(java.net.URL)
   */
  @Override
  public Feature[] parseFile( URL url ) throws Exception
  {
    LineNumberReader reader = new LineNumberReader( new InputStreamReader( url.openConnection().getInputStream() ) );
    final HashMap<String, Feature> nodeCollector = new HashMap<String, Feature>();
    readNet( reader, nodeCollector );
    readNodeList( reader );
    Collection<Feature> valueCol = nodeCollector.values();
    return valueCol.toArray( new Feature[valueCol.size()] );
  }

  /**
   * importing ascii part 2 : the nodeList
   */
  private void readNodeList( LineNumberReader reader ) throws Exception
  {
    String line;
    while( (line = reader.readLine()) != null )
    {
      if( line.startsWith( "9999" ) ) //$NON-NLS-1$
        return;

      final HashMap<String, String> propCollector = new HashMap<String, String>();
      final Map<IPropertyType, Object> fePropMap = new LinkedHashMap<IPropertyType, Object>();
      System.out.println( 3 + ": " + line ); //$NON-NLS-1$
      createProperties( propCollector, line, 2 );
      // final FeatureProperty knotProp = propCollector.get( "knot" );
      // final FeatureProperty izugProp = propCollector.get( "izug" );
      // final FeatureProperty iabgProp = propCollector.get( "iabg" );
      // final FeatureProperty iuebProp = propCollector.get( "iueb" );
      // final FeatureProperty izufProp = propCollector.get( "izuf" );
      // final FeatureProperty ivzwgProp = propCollector.get( "ivzwg" );

      int knot = Integer.parseInt( propCollector.get( "knot" ) ); //$NON-NLS-1$
      int izug = Integer.parseInt( propCollector.get( "izug" ) ); //$NON-NLS-1$
      int iabg = Integer.parseInt( propCollector.get( "iabg" ) ); //$NON-NLS-1$
      int iueb = Integer.parseInt( propCollector.get( "iueb" ) ); //$NON-NLS-1$
      int izuf = Integer.parseInt( propCollector.get( "izuf" ) ); //$NON-NLS-1$
      int ivzwg = Integer.parseInt( propCollector.get( "ivzwg" ) ); //$NON-NLS-1$

      final IFeatureType nodeFT = m_conf.getNodeFT();
      final Feature fe = getFeature( knot, nodeFT );
      if( izug > 0 ) // ZUGABE
      {
        throw new UnsupportedOperationException( Messages.getString( "org.kalypso.convert.namodel.manager.NetFileManager.8" ) ); //$NON-NLS-1$
        // TODO...
      }
      if( iabg > 0 ) // ABGABE
      {
        throw new UnsupportedOperationException( Messages.getString( "org.kalypso.convert.namodel.manager.NetFileManager.9" ) ); //$NON-NLS-1$
        // TODO...
      }
      if( iueb > 0 ) // UEBERLAUF
      {
        throw new UnsupportedOperationException( Messages.getString( "org.kalypso.convert.namodel.manager.NetFileManager.10" ) ); //$NON-NLS-1$
        // TODO...
      }
      if( izuf > 0 ) // ZUGABE oder ABGABE Kennlinie
      {
        if( izuf != 5 )
          throw new UnsupportedOperationException( Messages.getString( "org.kalypso.convert.namodel.manager.NetFileManager.11", izuf ) ); //$NON-NLS-1$
        line = reader.readLine();
        System.out.println( 6 + ": " + line ); //$NON-NLS-1$
        // da nur izuf==5 unterstuetzt wird ist zeile 6 nicht relevant

        line = reader.readLine();
        System.out.println( 7 + ": " + line ); //$NON-NLS-1$
        createProperties( propCollector, line, 7 );// nzufPfad
        String nzufPfad = propCollector.get( "nzufPfad" ); //$NON-NLS-1$
        // create timeserieslink

        String zmlPath = "Zufluss/Zufluss_" + fe.getId() + ".zml"; //$NON-NLS-1$ //$NON-NLS-2$

        // Was!?
        String correctedPath = nzufPfad.replaceAll( "P:\\\\vwe04121\\\\modell\\\\hydrologie\\\\namod\\\\zufluss\\\\", m_conf.getAsciiBaseDir().toString() + "/Zufluss/" ); //$NON-NLS-1$ //$NON-NLS-2$

        File tsFile = new File( correctedPath );
        final TimeseriesLinkType link1 = NAZMLGenerator.copyToTimeseriesLink( tsFile.toURL(), TimeserieConstants.TYPE_DATE, TimeserieConstants.TYPE_WATERLEVEL, m_conf.getGmlBaseDir(), zmlPath, false, false );

        final IPropertyType pt = nodeFT.getProperty( NaModelConstants.NODE_ZUFLUSS_ZR_REPOSITORY_PROP );
        fePropMap.put( pt, link1 );

        final TimeseriesLinkType link2 = NAZMLGenerator.copyToTimeseriesLink( tsFile.toURL(), TimeserieConstants.TYPE_DATE, TimeserieConstants.TYPE_WATERLEVEL, m_conf.getGmlBaseDir(), zmlPath, true, true );
        final IPropertyType pt2 = nodeFT.getProperty( NaModelConstants.NODE_ZUFLUSS_ZR_PROP );
        fePropMap.put( pt2, link2 );
      }
      if( ivzwg > 0 ) // VERZWEIGUNG
      {
        line = reader.readLine();
        System.out.println( 8 + ": " + line ); //$NON-NLS-1$
        createProperties( propCollector, line, 10 );// zproz ikz
        // resolve targetnode
        // FeatureProperty ikzProp = propCollector.get( "ikz" );
        int ikz = Integer.parseInt( propCollector.get( "ikz" ) ); //$NON-NLS-1$
        final Feature targetNodeFE = getFeature( ikz, nodeFT );
        final IPropertyType pt = nodeFT.getProperty( NaModelConstants.NODE_VERZW_MEMBER_PROP );
        fePropMap.put( pt, targetNodeFE.getId() );
      }
      Set set = propCollector.entrySet();
      for( Iterator iter = set.iterator(); iter.hasNext(); )
      {
        // Entry element = (Entry) iter.next();
        // System.out.println( element.getKey() + "=" + ((FeatureProperty) element.getValue()).getValue() );
      }

      // adding Timeseries links

      final TimeseriesLinkType pegelLink = NAZMLGenerator.copyToTimeseriesLink( null, TimeserieConstants.TYPE_DATE, TimeserieConstants.TYPE_WATERLEVEL, m_conf // TODO
      // NA_PEGEL
      .getGmlBaseDir(), "Pegel/Pegel_" + fe.getId() + ".zml", true, true ); //$NON-NLS-1$//$NON-NLS-2$
      final IPropertyType pt = nodeFT.getProperty( NaModelConstants.NODE_PEGEL_ZR_PROP );
      fePropMap.put( pt, pegelLink );

      final TimeseriesLinkType resultLink = NAZMLGenerator.copyToTimeseriesLink( null, TimeserieConstants.TYPE_DATE, TimeserieConstants.TYPE_RUNOFF, m_conf.getGmlBaseDir(), "Ergebnisse/Berechnet/Abfluss_" //$NON-NLS-1$
          + fe.getId() + ".zml", true, true ); //$NON-NLS-1$
      final IPropertyType pt2 = nodeFT.getProperty( NaModelConstants.NODE_RESULT_TIMESERIESLINK_PROP );
      fePropMap.put( pt2, resultLink );

      setParsedProperties( fe, propCollector, fePropMap );
    }
  }

  /**
   * importing ascii part 1 : the network
   */
  private void readNet( LineNumberReader reader, HashMap<String, Feature> nodeCollector ) throws Exception
  {
    final HashMap<String, String> propCollector = new HashMap<String, String>();
    String line;
    line = reader.readLine();
    if( line == null || line.startsWith( "9999" ) ) //$NON-NLS-1$
      return;
    if( line.startsWith( "\\" ) ) //$NON-NLS-1$
    {
      readNet( reader, nodeCollector );
      return;
    }
    System.out.println( 0 + ": " + line ); //$NON-NLS-1$
    createProperties( propCollector, line, 0 );
    // final FeatureProperty iteilProp = propCollector.get( "iteil" );
    // final FeatureProperty istrngProp = propCollector.get( "istrng" );
    // final FeatureProperty iknotoProp = propCollector.get( "iknoto" );
    // final FeatureProperty iknotuProp = propCollector.get( "iknotu" );
    int iteil = Integer.parseInt( propCollector.get( "iteil" ) ); //$NON-NLS-1$
    int istrngNr = Integer.parseInt( propCollector.get( "istrng" ) ); //$NON-NLS-1$
    int iknotoNr = Integer.parseInt( propCollector.get( "iknoto" ) ); //$NON-NLS-1$
    int iknotuNr = Integer.parseInt( propCollector.get( "iknotu" ) ); //$NON-NLS-1$
    // create node feature and
    // set node numbers

    // final FeatureProperty numPropertyKnotO = FeatureFactory.createFeatureProperty( "num", "" + iknotoNr );
    final Feature knotoFE = getFeature( iknotoNr, m_conf.getNodeFT() );
    nodeCollector.put( knotoFE.getId(), knotoFE );
    knotoFE.setProperty( NaModelConstants.GML_FEATURE_NAME_PROP, "" + iknotoNr ); //$NON-NLS-1$
    // final FeatureProperty numPropertyKnotU = FeatureFactory.createFeatureProperty( "num", "" + iknotuNr );
    final Feature knotuFE = getFeature( iknotuNr, m_conf.getNodeFT() );
    nodeCollector.put( knotuFE.getId(), knotuFE );
    knotuFE.setProperty( NaModelConstants.GML_FEATURE_NAME_PROP, "" + iknotuNr ); //$NON-NLS-1$
    // set node channel relations
    final Feature strangFE = getExistingFeature( istrngNr, new IFeatureType[] { m_conf.getKmChannelFT(), m_conf.getVChannelFT(), m_conf.getStChannelFT() } );
    // node -> strang
    if( strangFE == null )
      System.out.println( istrngNr );
    //
    else
    {
      // final FeatureProperty downStreamProp1 = FeatureFactory.createFeatureProperty( "downStreamChannelMember",
      // strangFE.getId() );
      knotoFE.setProperty( NaModelConstants.LINK_NODE_DOWNSTREAMCHANNEL, strangFE.getId() );
      // strang -> node
      // final FeatureProperty downStreamProp2 = FeatureFactory.createFeatureProperty( "downStreamNodeMember",
      // knotuFE.getId() );
      strangFE.setProperty( NaModelConstants.LINK_CHANNEL_DOWNSTREAMNODE, knotuFE.getId() );

      // Teilgebiete lesen
      for( int i = 0; i < iteil; i++ )
      {
        line = reader.readLine();
        final HashMap<String, String> col = new HashMap<String, String>();
        System.out.println( 1 + ": " + line ); //$NON-NLS-1$
        createProperties( col, line, 1 );
        // final FeatureProperty nteilProp = col.get( "nteil" );
        final int nteil = Integer.parseInt( col.get( "nteil" ) ); //$NON-NLS-1$
        final Feature teilgebFE = getFeature( nteil, m_conf.getCatchemtFT() );
        // final FeatureProperty downStreamProp = FeatureFactory.createFeatureProperty( "entwaesserungsStrangMember",
        // strangFE.getId() );
        teilgebFE.setProperty( NaModelConstants.LINK_CATCHMENT_CHANNEL, strangFE.getId() );
      }
      //
    }
    readNet( reader, nodeCollector );
  }

  /**
   * generate NetElements for rrm model
   * 
   * @param workspace
   *          the rrm workspace
   * @param synthNWorkspace
   *          the synth precipitation workspace
   * @return a HashMap containing Channel-FeatureID (key) and NetElements (value)
   */
  public HashMap<String, NetElement> generateNetElements( final GMLWorkspace workspace, final GMLWorkspace synthNWorkspace ) throws Exception
  {
    final IFeatureType nodeFT = workspace.getGMLSchema().getFeatureType( NaModelConstants.NODE_ELEMENT_FT );
    final IFeatureType kontEntnahmeFT = workspace.getGMLSchema().getFeatureType( NaModelConstants.NODE_VERZW_ENTNAHME );
    // final IFeatureType kontZuflussFT = workspace.getFeatureType( "KontZufluss" );
    final IFeatureType ueberlaufFT = workspace.getGMLSchema().getFeatureType( NaModelConstants.NODE_VERZW_UEBERLAUF );
    final IFeatureType verzweigungFT = workspace.getGMLSchema().getFeatureType( NaModelConstants.NODE_VERZW_VERZWEIGUNG );
    // x -> rootNode
    // |
    // O -> virtueller Strang generiert NR xxx
    // |
    // x -> virtueller knoten generiert NR 10000

    // list of channels
    final List<Feature> channelList = new ArrayList<Feature>();
    // fill it
    final Feature[] vChannelFeatures = workspace.getFeatures( m_conf.getVChannelFT() );
    for( int i = 0; i < vChannelFeatures.length; i++ )
      channelList.add( vChannelFeatures[i] );
    final Feature[] kmChannelFeatures = workspace.getFeatures( m_conf.getKmChannelFT() );
    for( int i = 0; i < kmChannelFeatures.length; i++ )
      channelList.add( kmChannelFeatures[i] );
    final Feature[] stChannelFeatures = workspace.getFeatures( m_conf.getStChannelFT() );
    for( int i = 0; i < stChannelFeatures.length; i++ )
      channelList.add( stChannelFeatures[i] );

    // list of network elements
    final HashMap<String, NetElement> netElements = new HashMap<String, NetElement>();
    // generate net elements, each channel represents a netelement
    final Feature[] channelFEs = channelList.toArray( new Feature[channelList.size()] );
    for( int i = 0; i < channelFEs.length; i++ )

      netElements.put( channelFEs[i].getId(), new NetElement( this, workspace, synthNWorkspace, channelFEs[i], m_conf ) );

    // find dependencies
    // dependency: node - node
    final Feature[] nodeFEs = workspace.getFeatures( m_conf.getNodeFT() );
    for( int i = 0; i < nodeFEs.length; i++ )
    {
      final Feature upStreamNodeFE = nodeFEs[i];
      final IFeatureType upstreamFT = upStreamNodeFE.getFeatureType();
      final IRelationType rt = (IRelationType) upstreamFT.getProperty( NaModelConstants.LINK_NODE_DOWNSTREAMCHANNEL );
      final Feature upStreamChannelFE = workspace.resolveLink( upStreamNodeFE, rt );
      final IRelationType rt2 = (IRelationType) upstreamFT.getProperty( NaModelConstants.NODE_BRANCHING_MEMBER_PROP );
      final Feature branchingFE = workspace.resolveLink( upStreamNodeFE, rt2 );
      if( branchingFE != null )
      {
        final IFeatureType branchingFT = branchingFE.getFeatureType();
        if( branchingFT == kontEntnahmeFT || branchingFT == ueberlaufFT || branchingFT == verzweigungFT )
        {
          final IRelationType rt1 = (IRelationType) branchingFT.getProperty( NaModelConstants.NODE_BRANCHING_NODE_MEMBER_PROP );
          final Feature downStreamNodeFE = workspace.resolveLink( branchingFE, rt1 );

          final IRelationType rt3 = (IRelationType) nodeFT.getProperty( NaModelConstants.LINK_NODE_DOWNSTREAMCHANNEL );
          final Feature downStreamChannelFE = workspace.resolveLink( downStreamNodeFE, rt3 );
          if( upStreamChannelFE == downStreamChannelFE )
          {
            System.out.println( "impossible net at #" + upStreamChannelFE.getId() + "\n Node-Node relation to it self" ); //$NON-NLS-1$ //$NON-NLS-2$
            continue;
          }
          // set dependency
          final NetElement upStreamElement = netElements.get( upStreamChannelFE.getId() );
          final NetElement downStreamElement = netElements.get( downStreamChannelFE.getId() );
          downStreamElement.addUpStream( upStreamElement );
        }
      }
    }
    // dependency: channel - node

    for( int i = 0; i < channelFEs.length; i++ )
    {
      final Feature channel = channelFEs[i];
      IRelationType rt = (IRelationType) channel.getFeatureType().getProperty( NaModelConstants.LINK_CHANNEL_DOWNSTREAMNODE );
      final Feature downStreamNodeFE = workspace.resolveLink( channel, rt );
      if( downStreamNodeFE == null )
      {
        System.out.println( "Channel #" + channel.getId() + "is outside network" ); //$NON-NLS-1$ //$NON-NLS-2$
        continue;
      }
      final IRelationType rt2 = (IRelationType) downStreamNodeFE.getFeatureType().getProperty( NaModelConstants.LINK_NODE_DOWNSTREAMCHANNEL );
      final Feature downStreamChannelFE = workspace.resolveLink( downStreamNodeFE, rt2 );
      if( downStreamChannelFE == null )
      {
        System.out.println( "Node #" + downStreamNodeFE.getId() + " has no downstream connection" ); //$NON-NLS-1$ //$NON-NLS-2$
        continue;
      }
      // set dependency
      if( channel == downStreamChannelFE )
      {
        System.out.println( "impossible net at #" + channel.getId() + "\n channel discharges to it self" ); //$NON-NLS-1$ //$NON-NLS-2$
        continue;
      }

      final NetElement upStreamElement = netElements.get( channel.getId() );
      final NetElement downStreamElement = netElements.get( downStreamChannelFE.getId() );

      downStreamElement.addUpStream( upStreamElement );
    }
    // TODO check dependency storagechannel -> overflownode
    // dependency: catchment -> catchment
    Feature[] catchmentFEs = workspace.getFeatures( m_conf.getCatchemtFT() );
    for( int i = 0; i < catchmentFEs.length; i++ )
    {
      final Feature catchmentFE = catchmentFEs[i];
      // upstream
      final IRelationType rt = (IRelationType) catchmentFE.getFeatureType().getProperty( NaModelConstants.LINK_CATCHMENT_CHANNEL );
      final Feature upStreamFE = workspace.resolveLink( catchmentFE, rt );
      if( upStreamFE == null )
      {
        System.out.println( " Catchment #" + catchmentFE.getId() + " is not connected to network" ); //$NON-NLS-1$ //$NON-NLS-2$
        continue;
      }

      final NetElement upStreamElement = netElements.get( upStreamFE.getId() );
      // downstream
      final IRelationType rt1 = (IRelationType) catchmentFE.getFeatureType().getProperty( NaModelConstants.GRUNDWASSERABFLUSS_MEMBER );
      final Feature[] abflussFEs = workspace.resolveLinks( catchmentFE, rt1 );
      for( int j = 0; j < abflussFEs.length; j++ )
      {
        final Feature abflussFE = abflussFEs[j];
        final IRelationType rt2 = (IRelationType) abflussFE.getFeatureType().getProperty( NaModelConstants.CATCHMENT_PROP_NGWZU );
        final Feature downStreamCatchmentFE = workspace.resolveLink( abflussFE, rt2 );
        if( downStreamCatchmentFE == null )
        {
          Logger.getAnonymousLogger().log( Level.WARNING, String.format( "Downstream catchment for #%s cannot be resolved.", abflussFE.getId() ) ); //$NON-NLS-1$
          continue;
        }
        final IRelationType rt3 = (IRelationType) downStreamCatchmentFE.getFeatureType().getProperty( NaModelConstants.LINK_CATCHMENT_CHANNEL );
        final Feature downStreamChannelFE = workspace.resolveLink( downStreamCatchmentFE, rt3 );
        if( downStreamChannelFE == null )
        {
          Logger.getAnonymousLogger().log( Level.WARNING, String.format( "Catchment #%s is not connected to network.", downStreamCatchmentFE.getId() ) ); //$NON-NLS-1$
          continue;
        }
        final NetElement downStreamElement = netElements.get( downStreamChannelFE.getId() );
        if( downStreamElement == null )
        {
          System.out.println( " TODO" ); //$NON-NLS-1$
          continue;
        }

        if( upStreamFE == downStreamChannelFE )
        {
          // two catchments discharges to the same channel, no need to generate
          // dependency cause it is the same channel
          continue;
        }
        downStreamElement.addUpStream( upStreamElement );
      }
    }
    return netElements;
  }

  /**
   * writes netfile (ascii)
   * 
   * @param asciiBuffer
   *          buffer for output buffering
   * @param workspace
   *          rrm workspace
   * @param synthNWorkspace
   *          workspace for synthetic precipitation
   */
  public void writeFile( final AsciiBuffer asciiBuffer, final GMLWorkspace workspace, final GMLWorkspace synthNWorkspace ) throws Exception
  {
    final HashMap<String, NetElement> netElements = generateNetElements( workspace, synthNWorkspace );
    // collect netelements that are direct upstream of result nodes
    final RootNodeCollectorVisitor rootNodeVisitor;
    final Feature rootNodeFE = workspace.getFeature( m_conf.getRootNodeId() );
    if( rootNodeFE != null )
      rootNodeVisitor = new RootNodeCollectorVisitor( rootNodeFE );
    else
      rootNodeVisitor = new RootNodeCollectorVisitor();
    for( Iterator iter = netElements.values().iterator(); iter.hasNext(); )
    {
      NetElement element = (NetElement) iter.next();
      element.accept( rootNodeVisitor );
    }
    final List rootNetElements = rootNodeVisitor.getRootNodeElements();

    // write asciifiles: upstream-network of root nodes
    final WriteAsciiVisitor writeAsciiVisitor = new WriteAsciiVisitor( asciiBuffer );
    final SimulationVisitor simulationVisitor = new SimulationVisitor( writeAsciiVisitor );
    for( Iterator iter = rootNetElements.iterator(); iter.hasNext(); )
    {
      NetElement element = (NetElement) iter.next();
      simulationVisitor.visit( element );
    }

    // write ascii: complete network below root nodes
    final CompleteDownstreamNetAsciiWriterVisitor completeNetVisitor = new CompleteDownstreamNetAsciiWriterVisitor( asciiBuffer );
    for( Iterator iter = netElements.values().iterator(); iter.hasNext(); )
    {
      NetElement netElement = (NetElement) iter.next();
      netElement.accept( completeNetVisitor );
    }
    final List nodeCollector = writeAsciiVisitor.getNodeCollector();
    asciiBuffer.getNetBuffer().append( "99999\n" ); //$NON-NLS-1$
    appendNodeList( workspace, nodeCollector, asciiBuffer );
    asciiBuffer.getNetBuffer().append( "99999\n" ); //$NON-NLS-1$
  }

  public void appendNodeList( GMLWorkspace workspace, List nodeCollector, AsciiBuffer asciiBuffer ) throws Exception, Exception
  {
    final IFeatureType kontEntnahmeFT = workspace.getGMLSchema().getFeatureType( NaModelConstants.NODE_VERZW_ENTNAHME );
    final IFeatureType kontZuflussFT = workspace.getGMLSchema().getFeatureType( NaModelConstants.NODE_VERZW_ZUFLUSS );
    final IFeatureType ueberlaufFT = workspace.getGMLSchema().getFeatureType( NaModelConstants.NODE_VERZW_UEBERLAUF );
    final IFeatureType verzweigungFT = workspace.getGMLSchema().getFeatureType( NaModelConstants.NODE_VERZW_VERZWEIGUNG );

    final IDManager idManager = m_conf.getIdManager();
    final Iterator iter = nodeCollector.iterator();
    while( iter.hasNext() )
    {
      final Feature nodeFE = (Feature) iter.next();
      asciiBuffer.getNetBuffer().append( FortranFormatHelper.printf( idManager.getAsciiID( nodeFE ), "i5" ) ); //$NON-NLS-1$

      final int izug;
      final int iabg;
      final int iueb;
      final int izuf;
      final int ivzwg;

      final StringBuffer specialBuffer = new StringBuffer();

      final TimeseriesLinkType zuflussLink = (TimeseriesLinkType) nodeFE.getProperty( NaModelConstants.NODE_ZUFLUSS_ZR_PROP );
      final IFeatureType nodeFT = nodeFE.getFeatureType();
      final IRelationType rt = (IRelationType) nodeFT.getProperty( NaModelConstants.NODE_BRANCHING_MEMBER_PROP );
      final Feature branchingFE = workspace.resolveLink( nodeFE, rt );
      if( branchingFE != null )
      {
        final IRelationType branchingNodeMemberRT = (IRelationType) branchingFE.getFeatureType().getProperty( NaModelConstants.NODE_BRANCHING_NODE_MEMBER_PROP );
        final IFeatureType branchingFT = branchingFE.getFeatureType();
        if( branchingFT == kontZuflussFT )
        {
          izug = 1;
          iabg = 0;
          iueb = 0;
          izuf = 0;
          ivzwg = 0;
          final double qzug = FeatureHelper.getAsDouble( branchingFE, NaModelConstants.NODE_VERZW_QZUG_PROP, 0d );
          specialBuffer.append( FortranFormatHelper.printf( qzug, "f10.3" ) ); //$NON-NLS-1$
        }
        else if( branchingFT == verzweigungFT )
        {
          izug = 0;
          iabg = 0;
          iueb = 0;
          izuf = 0;
          ivzwg = 1;

          final Feature targetNodeFE = workspace.resolveLink( branchingFE, branchingNodeMemberRT );
          final double zproz = FeatureHelper.getAsDouble( branchingFE, NaModelConstants.NODE_VERZW_ZPROZ_PROP, 0d );
          specialBuffer.append( FortranFormatHelper.printf( zproz, "f10.3" ) ); //$NON-NLS-1$
          specialBuffer.append( FortranFormatHelper.printf( idManager.getAsciiID( targetNodeFE ), "i8" ) + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$
        }
        else if( branchingFT == kontEntnahmeFT )
        {
          izug = 0;
          iabg = 1;
          iueb = 0;
          izuf = 0;
          ivzwg = 0;
          final Feature targetNodeFE = workspace.resolveLink( branchingFE, branchingNodeMemberRT );
          final double qabg = FeatureHelper.getAsDouble( branchingFE, NaModelConstants.NODE_VERZW_QABG_PROP, 0d );
          specialBuffer.append( FortranFormatHelper.printf( qabg, "f10.3" ) ); //$NON-NLS-1$
          specialBuffer.append( FortranFormatHelper.printf( idManager.getAsciiID( targetNodeFE ), "i8" ) + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$
        }
        else if( branchingFT == ueberlaufFT )
        {
          izug = 0;
          iabg = 0;
          iueb = 1;
          izuf = 0;
          ivzwg = 0;
          final Feature targetNodeFE = workspace.resolveLink( branchingFE, branchingNodeMemberRT );
          final double queb = FeatureHelper.getAsDouble( branchingFE, NaModelConstants.NODE_VERZW_QUEB_PROP, 0d );
          specialBuffer.append( FortranFormatHelper.printf( queb, "f10.3" ) ); //$NON-NLS-1$
          specialBuffer.append( FortranFormatHelper.printf( idManager.getAsciiID( targetNodeFE ), "i8" ) + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$
        }
        else
        {
          izug = 0;
          iabg = 0;
          iueb = 0;
          izuf = 0;
          ivzwg = 0;
        }
      }
      else if( zuflussLink != null )
      {
        izug = 0;
        iabg = 0;
        iueb = 0;
        ivzwg = 0;
        izuf = 5;
        final String zuflussFileName = getZuflussEingabeDateiString( nodeFE, m_conf );
        final File targetFile = new File( m_conf.getAsciiBaseDir(), "zufluss/" + zuflussFileName ); //$NON-NLS-1$
        final File parent = targetFile.getParentFile();
        if( !parent.exists() )
          parent.mkdirs();
        final String zuflussFile = ZmlURL.getIdentifierPart( zuflussLink.getHref() );
        final URL linkURL = m_urlUtilities.resolveURL( workspace.getContext(), zuflussFile );
        if( !targetFile.exists() )
        {
          final FileWriter writer = new FileWriter( targetFile );
          final IObservation observation = ZmlFactory.parseXML( linkURL, "ID" ); //$NON-NLS-1$
          if( Boolean.TRUE.equals( nodeFE.getProperty( NaModelConstants.NODE_SYNTHETIC_ZUFLUSS_ZR_PROP ) ) )
          {
            if( m_conf.isUsePrecipitationForm() )
            {
              final ITuppleModel values = observation.getValues( null );
              final IAxis[] axis = observation.getAxisList();
              final IAxis dateAxis = ObservationUtilities.findAxisByType( axis, TimeserieConstants.TYPE_DATE );
              final long simulationStartDateMillis = ((Date) values.getElement( 0, dateAxis )).getTime();
              final long simulationEndDateMillis = ((Date) values.getElement( values.getCount() - 1, dateAxis )).getTime();
              final Date simulationStartDate = new Date( 100, 0, 1 );
              final Date simulationEndDate = new Date( simulationStartDate.getTime() + simulationEndDateMillis - simulationStartDateMillis );
              NAZMLGenerator.createSyntheticFile( writer, TimeserieConstants.TYPE_RUNOFF, observation, simulationStartDate, simulationEndDate, m_conf.getMinutesOfTimeStep() );
            }
            else
              NAZMLGenerator.createSyntheticFile( writer, TimeserieConstants.TYPE_RUNOFF, observation, m_conf.getSimulationStart(), m_conf.getSimulationEnd(), m_conf.getMinutesOfTimeStep() );
          }
          else
            NAZMLGenerator.createFile( writer, TimeserieConstants.TYPE_RUNOFF, observation );
          IOUtils.closeQuietly( writer );
        }
        specialBuffer.append( "    1234\n" ); // dummyLine //$NON-NLS-1$
        specialBuffer.append( ".." + File.separator + "zufluss" + File.separator + zuflussFileName + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
      }
      else
      {
        izug = 0;
        iabg = 0;
        iueb = 0;
        izuf = 0;
        ivzwg = 0;
      }

      asciiBuffer.getNetBuffer().append( FortranFormatHelper.printf( izug, "i5" ) ); //$NON-NLS-1$
      asciiBuffer.getNetBuffer().append( FortranFormatHelper.printf( iabg, "i5" ) ); //$NON-NLS-1$
      asciiBuffer.getNetBuffer().append( FortranFormatHelper.printf( iueb, "i5" ) ); //$NON-NLS-1$
      asciiBuffer.getNetBuffer().append( FortranFormatHelper.printf( izuf, "i5" ) ); //$NON-NLS-1$
      asciiBuffer.getNetBuffer().append( FortranFormatHelper.printf( ivzwg, "i5" ) + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$
      asciiBuffer.getNetBuffer().append( specialBuffer.toString() );
      writeQQRelation( nodeFE, asciiBuffer.getNetBuffer() );
      // ENDKNOTEN

    }
    asciiBuffer.getNetBuffer().append( " 9001    0    0    0    0    0\n" ); //$NON-NLS-1$
    asciiBuffer.getNetBuffer().append( "10000    0    0    0    0    0\n" ); //$NON-NLS-1$
  }

  private void writeQQRelation( final Feature node, final StringBuffer buffer ) throws SensorException
  {
    final String relatedNodeID = (String) node.getProperty( NaModelConstants.NODE_QQRELATED_NODE_PROP );
    final IObservation observation = (IObservation) node.getProperty( NaModelConstants.NODE_QQRELATION_PROP );
    if( relatedNodeID == null || observation == null || relatedNodeID.length() == 0 )
      return;
    final IAxis[] axisList = observation.getAxisList();
    final IAxis q1Axis = ObservationUtilities.findAxisByType( axisList, TimeserieConstants.TYPE_RUNOFF );
    final IAxis q2Axis = ObservationUtilities.findAxisByType( axisList, TimeserieConstants.TYPE_RUNOFF_RHB );
    final ITuppleModel values = observation.getValues( null );
    int count = values.getCount();
    if( count < 1 )
      return;
    buffer.append( String.format( "%5d %6s\n", count, relatedNodeID ) ); //$NON-NLS-1$
    for( int row = 0; row < count; row++ )
    {
      final double q1 = (Double) values.getElement( row, q1Axis );
      final double q2 = (Double) values.getElement( row, q2Axis );
      buffer.append( String.format( Locale.ENGLISH, "%8.3f %8.3f\n", q1, q2 ) ); //$NON-NLS-1$
    }
  }

  private String getZuflussEingabeDateiString( Feature nodeFE, NAConfiguration conf )
  {
    int asciiID = conf.getIdManager().getAsciiID( nodeFE );
    return "Z_" + Integer.toString( asciiID ).trim() + ".zufluss"; //$NON-NLS-1$ //$NON-NLS-2$
  }

}