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
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.Map.Entry;

import org.apache.commons.io.IOUtils;
import org.kalypso.contribs.java.net.UrlUtilities;
import org.kalypso.contribs.java.util.FortranFormatHelper;
import org.kalypso.convert.namodel.NAConfiguration;
import org.kalypso.convert.namodel.NaNodeResultProvider;
import org.kalypso.convert.namodel.net.NetElement;
import org.kalypso.convert.namodel.net.visitors.CompleteDownstreamNetAsciiWriterVisitor;
import org.kalypso.convert.namodel.net.visitors.RootNodeCollectorVisitor;
import org.kalypso.convert.namodel.net.visitors.SimulationVisitor;
import org.kalypso.convert.namodel.net.visitors.WriteAsciiVisitor;
import org.kalypso.convert.namodel.timeseries.NAZMLGenerator;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.ogc.sensor.zml.ZmlURL;
import org.kalypso.zml.obslink.TimeseriesLink;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureProperty;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * @author doemming
 * 
 * erstellt die netzdatei aus namodell.gml oder liest aus netzdatei und erstellt namodell.gml
 * 
 * ---- logik bei der netzerstellung:
 * 
 * wird im control ein rootnode angegeben, so wird das netz hierfuer erstellt. vorhandene ergebnisse oberhalb des
 * rootnode werden als zufluss gerechnet.
 * 
 * oder
 * 
 * wird im control kein rotnode angegeben so wird das netz fuer die zu generierenden ergebnisse erstellt vorhandene
 * ergebnisse oberhalb der zu berechneneden knoten werden als zufluss gerechnet. Wobei zu generierende Ergebnisse stets
 * neu berechnet werden und niemals als Zufluss dienen.
 */
public class NetFileManager extends AbstractManager
{
  public static boolean DEBUG = false;

  final public NAConfiguration m_conf;

  private final UrlUtilities m_urlUtilities;

  public NetFileManager( NAConfiguration conf ) throws IOException
  {
    super( conf.getNetFormatURL() );
    m_conf = conf;
    m_urlUtilities = new UrlUtilities();
  }

  public String mapID( int id, FeatureType ft )
  {
    return ft.getName() + id;
  }

  /**
   * importing ascii file to gml-modell
   * 
   * @see org.kalypso.convert.namodel.manager.AbstractManager#parseFile(java.net.URL)
   */
  public Feature[] parseFile( URL url ) throws Exception
  {
    LineNumberReader reader = new LineNumberReader( new InputStreamReader( url.openConnection().getInputStream() ) );
    HashMap nodeCollector = new HashMap();
    readNet( reader, nodeCollector );
    readNodeList( reader );
    Collection valueCol = nodeCollector.values();
    return (Feature[])valueCol.toArray( new Feature[valueCol.size()] );
  }

  /**
   * importing ascii part 2 : the nodeList
   */
  private void readNodeList( LineNumberReader reader ) throws Exception
  {
    String line;
    while( ( line = reader.readLine() ) != null )
    {
      if( line.startsWith( "9999" ) )
        return;

      HashMap propCollector = new HashMap();
      System.out.println( 3 + ": " + line );
      createProperties( propCollector, line, 2 );
      final FeatureProperty knotProp = (FeatureProperty)propCollector.get( "knot" );
      final FeatureProperty izugProp = (FeatureProperty)propCollector.get( "izug" );
      final FeatureProperty iabgProp = (FeatureProperty)propCollector.get( "iabg" );
      final FeatureProperty iuebProp = (FeatureProperty)propCollector.get( "iueb" );
      final FeatureProperty izufProp = (FeatureProperty)propCollector.get( "izuf" );
      final FeatureProperty ivzwgProp = (FeatureProperty)propCollector.get( "ivzwg" );
      int knot = Integer.parseInt( (String)knotProp.getValue() );
      int izug = Integer.parseInt( (String)izugProp.getValue() );
      int iabg = Integer.parseInt( (String)iabgProp.getValue() );
      int iueb = Integer.parseInt( (String)iuebProp.getValue() );
      int izuf = Integer.parseInt( (String)izufProp.getValue() );
      int ivzwg = Integer.parseInt( (String)ivzwgProp.getValue() );
      final Feature fe = getFeature( knot, m_conf.getNodeFT() );
      if( izug > 0 ) // ZUGABE
      {
        throw new UnsupportedOperationException( "Netzdatei: izug>0 wird nicht unterstuetzt" );
        //TODO...
      }
      if( iabg > 0 ) // ABGABE
      {
        throw new UnsupportedOperationException( "Netzdatei: iabg>0 wird nicht unterstuetzt" );
        //TODO...
      }
      if( iueb > 0 ) // UEBERLAUF
      {
        throw new UnsupportedOperationException( "Netzdatei: iueb>0 wird nicht unterstuetzt" );
        //TODO...
      }
      if( izuf > 0 ) // ZUGABE oder ABGABE Kennlinie
      {
        if( izuf != 5 )
          throw new UnsupportedOperationException( "Netzdatei: Kennziffer Zu- oder Abgabekennlinie izuf=" + izuf
              + " wird nicht unterstuetzt. Unterstuetzt werden izuf=0 und izuf=5." );
        line = reader.readLine();
        System.out.println( 6 + ": " + line );
        // da nur izuf==5 unterstuetzt wird ist zeile 6 nicht relevant

        line = reader.readLine();
        System.out.println( 7 + ": " + line );
        createProperties( propCollector, line, 7 );// nzufPfad
        String nzufPfad = (String)( (FeatureProperty)propCollector.get( "nzufPfad" ) ).getValue();
        // create timeserieslink

        String zmlPath = "Zufluss/Zufluss_" + fe.getId() + ".zml";
        String correctedPath = nzufPfad.replaceAll( "P:\\\\vwe04121\\\\modell\\\\hydrologie\\\\namod\\\\zufluss\\\\",
            m_conf.getAsciiBaseDir().toString() + "/Zufluss/" );
        File tsFile = new File( correctedPath );
        TimeseriesLink link1 = NAZMLGenerator.copyToTimeseriesLink( tsFile.toURL(), TimeserieConstants.TYPE_DATE,
            TimeserieConstants.TYPE_WATERLEVEL, m_conf.getGmlBaseDir(), zmlPath, false, false );
        FeatureProperty linkPropertyRepository = FeatureFactory.createFeatureProperty( "zuflussZRRepository", link1 );
        propCollector.put( "zuflussZRRepository", linkPropertyRepository );

        TimeseriesLink link2 = NAZMLGenerator.copyToTimeseriesLink( tsFile.toURL(), TimeserieConstants.TYPE_DATE,
            TimeserieConstants.TYPE_WATERLEVEL, m_conf.getGmlBaseDir(), zmlPath, true, true );
        FeatureProperty linkProperty = FeatureFactory.createFeatureProperty( "zuflussZR", link2 );
        propCollector.put( "zuflussZR", linkProperty );
      }
      if( ivzwg > 0 ) // VERZWEIGUNG
      {
        line = reader.readLine();
        System.out.println( 8 + ": " + line );
        createProperties( propCollector, line, 10 );// zproz ikz
        // resolve targetnode
        FeatureProperty ikzProp = (FeatureProperty)propCollector.get( "ikz" );
        int ikz = Integer.parseInt( (String)ikzProp.getValue() );
        Feature targetNodeFE = getFeature( ikz, m_conf.getNodeFT() );
        FeatureProperty linkedNodeProp = FeatureFactory.createFeatureProperty( "verzweigungNodeMember", targetNodeFE
            .getId() );
        propCollector.put( "verzweigungNodeMember", linkedNodeProp );
      }
      Set set = propCollector.entrySet();
      for( Iterator iter = set.iterator(); iter.hasNext(); )
      {
        Entry element = (Entry)iter.next();
        System.out.println( element.getKey() + "=" + ( (FeatureProperty)element.getValue() ).getValue() );
      }

      // adding Timeseries links

      final TimeseriesLink pegelLink = NAZMLGenerator.copyToTimeseriesLink( null, TimeserieConstants.TYPE_DATE,
          TimeserieConstants.TYPE_WATERLEVEL, m_conf // TODO
              // NA_PEGEL
              .getGmlBaseDir(), "Pegel/Pegel_" + fe.getId() + ".zml", true, true );
      FeatureProperty pegelProp = FeatureFactory.createFeatureProperty( "pegelZR", pegelLink );
      propCollector.put( "pegelZR", pegelProp );

      final TimeseriesLink resultLink = NAZMLGenerator.copyToTimeseriesLink( null, TimeserieConstants.TYPE_DATE,
          TimeserieConstants.TYPE_RUNOFF, m_conf.getGmlBaseDir(),
          "Ergebnisse/Berechnet/Abfluss_" + fe.getId() + ".zml", true, true );
      FeatureProperty ergProp = FeatureFactory.createFeatureProperty( "qberechnetZR", resultLink );
      propCollector.put( "qberechnetZR", ergProp );

      setParsedProperties( fe, propCollector.values() );
    }
  }

  /**
   * importing ascii part 1 : the network
   */
  private void readNet( LineNumberReader reader, HashMap nodeCollector ) throws Exception
  {
    HashMap propCollector = new HashMap();
    String line;
    line = reader.readLine();
    if( line == null || line.startsWith( "9999" ) )
      return;
    if( line.startsWith( "\\" ) )
    {
      readNet( reader, nodeCollector );
      return;
    }
    System.out.println( 0 + ": " + line );
    createProperties( propCollector, line, 0 );
    final FeatureProperty iteilProp = (FeatureProperty)propCollector.get( "iteil" );
    final FeatureProperty istrngProp = (FeatureProperty)propCollector.get( "istrng" );
    final FeatureProperty iknotoProp = (FeatureProperty)propCollector.get( "iknoto" );
    final FeatureProperty iknotuProp = (FeatureProperty)propCollector.get( "iknotu" );
    int iteil = Integer.parseInt( (String)iteilProp.getValue() );
    int istrngNr = Integer.parseInt( (String)istrngProp.getValue() );
    int iknotoNr = Integer.parseInt( (String)iknotoProp.getValue() );
    int iknotuNr = Integer.parseInt( (String)iknotuProp.getValue() );
    // create node feature and
    // set node numbers

    final FeatureProperty numPropertyKnotO = FeatureFactory.createFeatureProperty( "num", "" + iknotoNr );
    final Feature knotoFE = getFeature( iknotoNr, m_conf.getNodeFT() );
    nodeCollector.put( knotoFE.getId(), knotoFE );
    knotoFE.setProperty( numPropertyKnotO );
    final FeatureProperty numPropertyKnotU = FeatureFactory.createFeatureProperty( "num", "" + iknotuNr );
    final Feature knotuFE = getFeature( iknotuNr, m_conf.getNodeFT() );
    nodeCollector.put( knotuFE.getId(), knotuFE );
    knotuFE.setProperty( numPropertyKnotU );
    // set node channel relations
    final Feature strangFE = getExistingFeature( istrngNr, new FeatureType[]
    {
        m_conf.getKmChannelFT(),
        m_conf.getVChannelFT(),
        m_conf.getStChannelFT() } );
    // node -> strang
    if( strangFE == null )
      System.out.println( istrngNr );
    //
    else
    {
      final FeatureProperty downStreamProp1 = FeatureFactory.createFeatureProperty( "downStreamChannelMember", strangFE
          .getId() );
      knotoFE.setProperty( downStreamProp1 );
      // strang -> node
      final FeatureProperty downStreamProp2 = FeatureFactory.createFeatureProperty( "downStreamNodeMember", knotuFE
          .getId() );
      strangFE.setProperty( downStreamProp2 );

      // Teilgebiete lesen
      for( int i = 0; i < iteil; i++ )
      {
        line = reader.readLine();
        final HashMap col = new HashMap();
        System.out.println( 1 + ": " + line );
        createProperties( col, line, 1 );
        final FeatureProperty nteilProp = (FeatureProperty)col.get( "nteil" );
        final int nteil = Integer.parseInt( (String)nteilProp.getValue() );
        final Feature teilgebFE = getFeature( nteil, m_conf.getCatchemtFT() );
        final FeatureProperty downStreamProp = FeatureFactory.createFeatureProperty( "entwaesserungsStrangMember",
            strangFE.getId() );
        teilgebFE.setProperty( downStreamProp );
      }
      //
    }
    readNet( reader, nodeCollector );
  }

  /*
   *  
   */
  public void writeFile( AsciiBuffer asciiBuffer, GMLWorkspace workspace, final NaNodeResultProvider nodeResultProvider )
      throws Exception
  {
    // to remove yellow thing ;-)
    nodeResultProvider.getClass();

    final FeatureType kontEntnahmeFT = workspace.getFeatureType( "KontEntnahme" );
    //    final FeatureType kontZuflussFT = workspace.getFeatureType( "KontZufluss" );
    final FeatureType ueberlaufFT = workspace.getFeatureType( "Ueberlauf" );
    final FeatureType verzweigungFT = workspace.getFeatureType( "Verzweigung" );
    //    x -> rootNode
    //    |
    //    O -> virtueller Strang generiert NR xxx
    //    |
    //    x -> virtueller knoten generiert NR 10000

    // list of channels
    final List channelList = new ArrayList();
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
    final HashMap netElements = new HashMap();
    // generate net elements, each channel represents a netelement
    final Feature[] channelFEs = (Feature[])channelList.toArray( new Feature[channelList.size()] );
    for( int i = 0; i < channelFEs.length; i++ )
      netElements.put( channelFEs[i].getId(), new NetElement( this, workspace, channelFEs[i], m_conf ) );

    // find dependencies
    //   dependency: node - node
    final Feature[] nodeFEs = workspace.getFeatures( m_conf.getNodeFT() );
    for( int i = 0; i < nodeFEs.length; i++ )
    {
      final Feature upStreamNodeFE = nodeFEs[i];
      final Feature upStreamChannelFE = workspace.resolveLink( upStreamNodeFE, "downStreamChannelMember" );
      final Feature branchingFE = workspace.resolveLink( upStreamNodeFE, "branchingMember" );
      if( branchingFE != null )
      {
        final FeatureType branchingFT = branchingFE.getFeatureType();
        if( branchingFT == kontEntnahmeFT || branchingFT == ueberlaufFT || branchingFT == verzweigungFT )
        {
          final Feature downStreamNodeFE = workspace.resolveLink( branchingFE, "branchingNodeMember" );
          final Feature downStreamChannelFE = workspace.resolveLink( downStreamNodeFE, "downStreamChannelMember" );
          if( upStreamChannelFE == downStreamChannelFE )
          {
            System.out.println( "impossible net at #" + upStreamChannelFE.getId() + "\n Node-Node relation to it self" );
            continue;
          }
          // set dependency
          final NetElement upStreamElement = (NetElement)netElements.get( upStreamChannelFE.getId() );
          final NetElement downStreamElement = (NetElement)netElements.get( downStreamChannelFE.getId() );
          downStreamElement.addUpStream( upStreamElement );
        }
      }
    }
    //   dependency: channel - node

    for( int i = 0; i < channelFEs.length; i++ )
    {
      final Feature channel = channelFEs[i];
      final Feature downStreamNodeFE = workspace.resolveLink( channel, "downStreamNodeMember" );
      if( downStreamNodeFE == null )
      {
        System.out.println( "Channel #" + channel.getId() + "is outside network" );
        continue;
      }
      final Feature downStreamChannelFE = workspace.resolveLink( downStreamNodeFE, "downStreamChannelMember" );
      if( downStreamChannelFE == null )
      {
        System.out.println( "Node #" + downStreamNodeFE.getId() + " has no downstream connection" );
        continue;
      }
      // set dependency
      if( channel == downStreamChannelFE )
      {
        System.out.println( "impossible net at #" + channel.getId() + "\n channel discharges to it self" );
        continue;
      }

      final NetElement upStreamElement = (NetElement)netElements.get( channel.getId() );
      final NetElement downStreamElement = (NetElement)netElements.get( downStreamChannelFE.getId() );

      downStreamElement.addUpStream( upStreamElement );
    }
    // TODO check dependency storagechannel -> overflownode
    //   dependency: catchment -> catchment
    Feature[] catchmentFEs = workspace.getFeatures( m_conf.getCatchemtFT() );
    for( int i = 0; i < catchmentFEs.length; i++ )
    {
      final Feature catchmentFE = catchmentFEs[i];
      // upstream
      final Feature upStreamFE = workspace.resolveLink( catchmentFE, "entwaesserungsStrangMember" );
      if( upStreamFE == null )
      {
        System.out.println( " Catchment #" + catchmentFE.getId() + " is not connected to network" );
        continue;
      }

      final NetElement upStreamElement = (NetElement)netElements.get( upStreamFE.getId() );
      // downstream
      final Feature[] abflussFEs = workspace.resolveLinks( catchmentFE, "grundwasserabflussMember" );
      for( int j = 0; j < abflussFEs.length; j++ )
      {
        final Feature abflussFE = abflussFEs[j];
        final Feature downStreamCatchmentFE = workspace.resolveLink( abflussFE, "ngwzu" );
        final Feature downStreamChannelFE = workspace.resolveLink( downStreamCatchmentFE, "entwaesserungsStrangMember" );
        if( downStreamChannelFE == null )
        {
          System.out.println( " Catchment #" + downStreamCatchmentFE.getId() + " is not connected to network" );
          continue;
        }
        final NetElement downStreamElement = (NetElement)netElements.get( downStreamChannelFE.getId() );
        if( downStreamElement == null )
        {
          System.out.println( " TODO" );
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

    //
    // write asciifiles
    //

    // collect netelements that are direct upstream of result nodes
    final RootNodeCollectorVisitor rootNodeVisitor;
    final Feature rootNodeFE = workspace.getFeature( m_conf.getRootNodeId() );
    if( rootNodeFE != null )
      rootNodeVisitor = new RootNodeCollectorVisitor( rootNodeFE );
    else
      rootNodeVisitor = new RootNodeCollectorVisitor();
    for( Iterator iter = netElements.values().iterator(); iter.hasNext(); )
    {
      NetElement element = (NetElement)iter.next();
      element.accept( rootNodeVisitor );
    }
    final List rootNetElements = rootNodeVisitor.getRootNodeElements();

    // write asciifiles: upstream-network of root nodes
    final WriteAsciiVisitor writeAsciiVisitor = new WriteAsciiVisitor( asciiBuffer );
    final SimulationVisitor simulationVisitor = new SimulationVisitor( writeAsciiVisitor );
    for( Iterator iter = rootNetElements.iterator(); iter.hasNext(); )
    {
      NetElement element = (NetElement)iter.next();
      simulationVisitor.visit( element );
    }

    // write ascii: complete network below root nodes
    final CompleteDownstreamNetAsciiWriterVisitor completeNetVisitor = new CompleteDownstreamNetAsciiWriterVisitor(
        asciiBuffer );
    for( Iterator iter = netElements.values().iterator(); iter.hasNext(); )
    {
      NetElement netElement = (NetElement)iter.next();
      netElement.accept( completeNetVisitor );
    }
    final List nodeCollector = writeAsciiVisitor.getNodeCollector();
    asciiBuffer.getNetBuffer().append( "99999\n" );
    appendNodeList( workspace, nodeCollector, asciiBuffer );
    asciiBuffer.getNetBuffer().append( "99999\n" );
  }

  public void appendNodeList( GMLWorkspace workspace, List nodeCollector, AsciiBuffer asciiBuffer ) throws Exception,
      Exception
  {
    final FeatureType kontEntnahmeFT = workspace.getFeatureType( "KontEntnahme" );
    final FeatureType kontZuflussFT = workspace.getFeatureType( "KontZufluss" );
    final FeatureType ueberlaufFT = workspace.getFeatureType( "Ueberlauf" );
    final FeatureType verzweigungFT = workspace.getFeatureType( "Verzweigung" );

    final IDManager idManager = m_conf.getIdManager();
    final Iterator iter = nodeCollector.iterator();
    while( iter.hasNext() )
    {
      final Feature nodeFE = (Feature)iter.next();
      asciiBuffer.getNetBuffer().append( FortranFormatHelper.printf( idManager.getAsciiID( nodeFE ), "i5" ) );

      final int izug;
      final int iabg;
      final int iueb;
      final int izuf;
      final int ivzwg;

      final StringBuffer specialBuffer = new StringBuffer();

      final TimeseriesLink zuflussLink = (TimeseriesLink)nodeFE.getProperty( "zuflussZR" );
      final Feature branchingFE = workspace.resolveLink( nodeFE, "branchingMember" );

      if( branchingFE != null )
      {
        final FeatureType branchingFT = branchingFE.getFeatureType();
        if( branchingFT == kontZuflussFT )
        {
          izug = 1;
          iabg = 0;
          iueb = 0;
          izuf = 0;
          ivzwg = 0;
          final double qzug = FeatureHelper.getAsDouble( branchingFE, "qzug", 0d );
          specialBuffer.append( FortranFormatHelper.printf( qzug, "f10.3" ) );
        }
        else if( branchingFT == verzweigungFT )
        {
          izug = 0;
          iabg = 0;
          iueb = 0;
          izuf = 0;
          ivzwg = 1;
          final Feature targetNodeFE = workspace.resolveLink( branchingFE, "branchingNodeMember" );
          final double zproz = FeatureHelper.getAsDouble( branchingFE, "zproz", 0d );
          specialBuffer.append( FortranFormatHelper.printf( zproz, "f10.3" ) );
          specialBuffer.append( FortranFormatHelper.printf( idManager.getAsciiID( targetNodeFE ), "i8" ) + "\n" );
        }
        else if( branchingFT == kontEntnahmeFT )
        {
          izug = 0;
          iabg = 1;
          iueb = 0;
          izuf = 0;
          ivzwg = 0;
          final Feature targetNodeFE = workspace.resolveLink( branchingFE, "branchingNodeMember" );
          final double qabg = FeatureHelper.getAsDouble( branchingFE, "qabg", 0d );
          specialBuffer.append( FortranFormatHelper.printf( qabg, "f10.3" ) );
          specialBuffer.append( FortranFormatHelper.printf( idManager.getAsciiID( targetNodeFE ), "i8" ) + "\n" );
        }
        else if( branchingFT == ueberlaufFT )
        {
          izug = 0;
          iabg = 0;
          iueb = 1;
          izuf = 0;
          ivzwg = 0;
          final Feature targetNodeFE = workspace.resolveLink( branchingFE, "branchingNodeMember" );
          final double queb = FeatureHelper.getAsDouble( branchingFE, "queb", 0d );
          specialBuffer.append( FortranFormatHelper.printf( queb, "f10.3" ) );
          specialBuffer.append( FortranFormatHelper.printf( idManager.getAsciiID( targetNodeFE ), "i8" ) + "\n" );
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
        final File targetFile = new File( m_conf.getAsciiBaseDir(), "zufluss/" + zuflussFileName );
        final File parent = targetFile.getParentFile();
        if( !parent.exists() )
          parent.mkdirs();
        final String zuflussFile = ZmlURL.getIdentifierPart( zuflussLink.getHref() );
        final URL linkURL = m_urlUtilities.resolveURL( workspace.getContext(), zuflussFile );
        if( !targetFile.exists() )
        {
          final IObservation observation = ZmlFactory.parseXML( linkURL, "ID" );
          final FileWriter writer = new FileWriter( targetFile );
          NAZMLGenerator.createFile( writer, TimeserieConstants.TYPE_RUNOFF, observation );
          IOUtils.closeQuietly( writer );
        }
        specialBuffer.append( "    1234\n" ); // dummyLine
        specialBuffer.append( ".." + File.separator + "zufluss" + File.separator + zuflussFileName + "\n" );
      }
      else
      {
        izug = 0;
        iabg = 0;
        iueb = 0;
        izuf = 0;
        ivzwg = 0;
      }

      asciiBuffer.getNetBuffer().append( FortranFormatHelper.printf( izug, "i5" ) );
      asciiBuffer.getNetBuffer().append( FortranFormatHelper.printf( iabg, "i5" ) );
      asciiBuffer.getNetBuffer().append( FortranFormatHelper.printf( iueb, "i5" ) );
      asciiBuffer.getNetBuffer().append( FortranFormatHelper.printf( izuf, "i5" ) );
      asciiBuffer.getNetBuffer().append( FortranFormatHelper.printf( ivzwg, "i5" ) + "\n" );
      asciiBuffer.getNetBuffer().append( specialBuffer.toString() );
      // ENDKNOTEN

    }
    asciiBuffer.getNetBuffer().append( " 9001    0    0    0    0    0\n" );
    asciiBuffer.getNetBuffer().append( "10000    0    0    0    0    0\n" );
  }

  private String getZuflussEingabeDateiString( Feature nodeFE, NAConfiguration conf )
  {
    int asciiID = conf.getIdManager().getAsciiID( nodeFE );
    return "Z_" + Integer.toString( asciiID ).trim() + ".zufluss";
  }

}