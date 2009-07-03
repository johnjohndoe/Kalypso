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
package org.kalypso.convert.namodel;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.Map.Entry;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureProperty;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.GMLWorkspace;
import org.deegree_impl.model.feature.FeatureFactory;
import org.deegree_impl.model.feature.FeatureHelper;
import org.kalypso.java.net.UrlUtilities;
import org.kalypso.java.util.FortranFormatHelper;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.ogc.sensor.zml.ZmlURL;
import org.kalypso.zml.obslink.TimeseriesLink;

/**
 * @author doemming
 * 
 * erstellt die netzdatei aus namodell.gml oder liest aus netzdatei und erstellt
 * namodell.gml
 * 
 * ---- logik bei der netzerstellung:
 * 
 * wird im control ein rootnode angegeben, so wird das netz hierfuer erstellt.
 * vorhandene ergebnisse oberhalb des rootnode werden als zufluss gerechnet.
 * 
 * oder
 * 
 * wird im control kein rotnode angegeben so wird das netz fuer die zu
 * generierenden ergebnisse erstellt vorhandene ergebnisse oberhalb der zu
 * berechneneden knoten werden als zufluss gerechnet. Wobei zu generierende
 * Ergebnisse stets neu berechnet werden und niemals als Zufluss dienen.
 */
public class NetFileManager extends AbstractManager
{
  static boolean DEBUG = false;//true; //TODO auf false setzen

  final NAConfiguration m_conf;

  private final UrlUtilities m_urlUtilities;

  protected File getResultFile( Feature nodeFE ) throws MalformedURLException
  {
    final TimeseriesLink link = (TimeseriesLink)nodeFE.getProperty( "qberechnetZR" );
    if( link == null )
      return null;
    final String href = link.getHref().replaceAll( "\\?.*", "" ); // optionen
    // loeschen

    final URL url = m_urlUtilities.resolveURL( m_conf.getGMLModelURL(), href );
    return new File( url.getFile() );
  }

  protected boolean resultExists( Feature nodeFE )
  {
    try
    {
      final File resultFile = getResultFile( nodeFE );
      if( resultFile == null )
        return false;
      return resultFile.exists();
    }
    catch( MalformedURLException e )
    {
      return false;
    }
  }

  public NetFileManager( NAConfiguration conf ) throws IOException
  {
    super( conf.getNetFormatURL() );
    m_conf = conf;
    m_urlUtilities = new UrlUtilities();
    NetElement.setNetAsciiFormats( getAsciiFormats() );
  }

  public String mapID( int id, FeatureType ft )
  {
    return ft.getName() + id;
  }

  /**
   * 
   * importing ascii file to gml-modell
   * 
   * @see org.kalypso.convert.namodel.AbstractManager#parseFile(java.net.URL)
   */
  public Feature[] parseFile( URL url ) throws Exception
  {
    LineNumberReader reader = new LineNumberReader( new InputStreamReader( url.openConnection()
        .getInputStream() ) );
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
          throw new UnsupportedOperationException(
              "Netzdatei: Kennziffer Zu- oder Abgabekennlinie izuf=" + izuf
                  + " wird nicht unterstuetzt. Unterstuetzt werden izuf=0 und izuf=5." );
        line = reader.readLine();
        System.out.println( 6 + ": " + line );
        // da nur izuf==5 unterstuetzt wird ist zeile 6 nicht relevant
        //  createProperties( propCollector, line, 6 );// nzufKnoten (Knoten aus
        // zuflussdatei)
        line = reader.readLine();
        System.out.println( 7 + ": " + line );
        createProperties( propCollector, line, 7 );// nzufPfad
        String nzufPfad = (String)( (FeatureProperty)propCollector.get( "nzufPfad" ) ).getValue();
        // create timeserieslink

        String zmlPath = "Zufluss/Zufluss_" + fe.getId() + ".zml";
        String correctedPath = nzufPfad.replaceAll(
            "P:\\\\vwe04121\\\\modell\\\\hydrologie\\\\namod\\\\zufluss\\\\", m_conf
                .getAsciiBaseDir().toString()
                + "/Zufluss/" );
        File tsFile = new File( correctedPath );
        TimeseriesLink link1 = NAZMLGenerator.copyToTimeseriesLink( tsFile.toURL(),
            TimeserieConstants.TYPE_DATE, TimeserieConstants.TYPE_WATERLEVEL, m_conf
                .getGmlBaseDir(), zmlPath, false, false );
        FeatureProperty linkPropertyRepository = FeatureFactory.createFeatureProperty(
            "zuflussZRRepository", link1 );
        propCollector.put( "zuflussZRRepository", linkPropertyRepository );

        TimeseriesLink link2 = NAZMLGenerator.copyToTimeseriesLink( tsFile.toURL(),
            TimeserieConstants.TYPE_DATE, TimeserieConstants.TYPE_WATERLEVEL, m_conf
                .getGmlBaseDir(), zmlPath, true, true );
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
        FeatureProperty linkedNodeProp = FeatureFactory.createFeatureProperty(
            "verzweigungNodeMember", targetNodeFE.getId() );
        propCollector.put( "verzweigungNodeMember", linkedNodeProp );
      }
      Set set = propCollector.entrySet();
      for( Iterator iter = set.iterator(); iter.hasNext(); )
      {
        Entry element = (Entry)iter.next();
        System.out.println( element.getKey() + "="
            + ( (FeatureProperty)element.getValue() ).getValue() );
      }

      // adding Timeseries links

      final TimeseriesLink pegelLink = NAZMLGenerator.copyToTimeseriesLink( null,
          TimeserieConstants.TYPE_DATE, TimeserieConstants.TYPE_WATERLEVEL, m_conf // TODO
                                                                                   // NA_PEGEL
              .getGmlBaseDir(), "Pegel/Pegel_" + fe.getId() + ".zml", true, true );
      FeatureProperty pegelProp = FeatureFactory.createFeatureProperty( "pegelZR", pegelLink );
      propCollector.put( "pegelZR", pegelProp );

      final TimeseriesLink resultLink = NAZMLGenerator.copyToTimeseriesLink( null,
          TimeserieConstants.TYPE_DATE, TimeserieConstants.TYPE_RUNOFF, m_conf.getGmlBaseDir(),
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
    final FeatureProperty numPropertyKnotO = FeatureFactory.createFeatureProperty( "num", ""
        + iknotoNr );
    final Feature knotoFE = getFeature( iknotoNr, m_conf.getNodeFT() );
    nodeCollector.put( knotoFE.getId(), knotoFE );
    knotoFE.setProperty( numPropertyKnotO );
    final FeatureProperty numPropertyKnotU = FeatureFactory.createFeatureProperty( "num", ""
        + iknotuNr );
    final Feature knotuFE = getFeature( iknotuNr, m_conf.getNodeFT() );
    nodeCollector.put( knotuFE.getId(), knotuFE );
    knotuFE.setProperty( numPropertyKnotU );
    // set node channel relations
    final Feature strangFE = getExistingFeature( istrngNr, new FeatureType[]
    {
        m_conf.getKmChannelFT(),
        m_conf.getVChannelFT() } );
    // node -> strang
    if( strangFE == null )
      System.out.println( istrngNr );
    //
    else
    {
      final FeatureProperty downStreamProp1 = FeatureFactory.createFeatureProperty(
          "downStreamChannelMember", strangFE.getId() );
      knotoFE.setProperty( downStreamProp1 );
      // strang -> node
      final FeatureProperty downStreamProp2 = FeatureFactory.createFeatureProperty(
          "downStreamNodeMember", knotuFE.getId() );
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
        final FeatureProperty downStreamProp = FeatureFactory.createFeatureProperty(
            "entwaesserungsStrangMember", strangFE.getId() );
        teilgebFE.setProperty( downStreamProp );
      }
      //
    }
    readNet( reader, nodeCollector );
  }

  /*
   * generate ascii netfile from gml workspace
   * 
   * @see org.kalypso.convert.namodel.AbstractManager#writeFile(java.io.Writer,
   *      org.deegree.model.feature.GMLWorkspace)
   */
  public void writeFile( AsciiBuffer asciiBuffer, GMLWorkspace workspace ) throws Exception
  {
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

    // list of network elements
    final HashMap netElements = new HashMap();
    NetElement.setDefaultGmlWorkSpace( workspace );
    // generate net elements, each channel represents a netelement
    final Feature[] channelFEs = (Feature[])channelList.toArray( new Feature[channelList.size()] );
    for( int i = 0; i < channelFEs.length; i++ )
      netElements.put( channelFEs[i].getId(), new NetElement( this, channelFEs[i] ) );

    // find dependencies
    //   dependency: node - node
    final Feature[] nodeFEs = workspace.getFeatures( m_conf.getNodeFT() );
    for( int i = 0; i < nodeFEs.length; i++ )
    {
      Feature upStreamNodeFE = nodeFEs[i];
      Feature upStreamChannelFE = workspace.resolveLink( upStreamNodeFE, "downStreamChannelMember" );
      Feature downStreamNodeFE = workspace.resolveLink( upStreamNodeFE, "verzweigungNodeMember" );
      if( downStreamNodeFE == null )
        continue;
      Feature downStreamChannelFE = workspace.resolveLink( downStreamNodeFE,
          "downStreamChannelMember" );
      // search upstreamchannel
      // set dependency
      if( upStreamChannelFE == downStreamChannelFE )
      {
        System.out.println( "impossible net at #" + upStreamChannelFE.getId() );
        continue;
      }

      NetElement upStreamElement = (NetElement)netElements.get( upStreamChannelFE.getId() );
      NetElement downStreamElement = (NetElement)netElements.get( downStreamChannelFE.getId() );
      downStreamElement.addUpStream( upStreamElement );
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
      final Feature downStreamChannelFE = workspace.resolveLink( downStreamNodeFE,
          "downStreamChannelMember" );
      if( downStreamChannelFE == null )
      {
        System.out.println( "Node #" + downStreamNodeFE.getId() + " has no downstream connection" );
        continue;
      }
      // set dependency
      if( channel == downStreamChannelFE )
      {
        System.out.println( "impossible net at #" + channel.getId() );
        continue;
      }

      final NetElement upStreamElement = (NetElement)netElements.get( channel.getId() );
      final NetElement downStreamElement = (NetElement)netElements
          .get( downStreamChannelFE.getId() );

      downStreamElement.addUpStream( upStreamElement );
    }
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
        final Feature downStreamChannelFE = workspace.resolveLink( downStreamCatchmentFE,
            "entwaesserungsStrangMember" );
        if( downStreamChannelFE == null )
        {
          System.out.println( " Catchment #" + downStreamCatchmentFE.getId()
              + " is not connected to network" );
          continue;
        }
        final NetElement downStreamElement = (NetElement)netElements.get( downStreamChannelFE
            .getId() );
        if( downStreamElement == null )
        {
          System.out.println( " TODO" );
          continue;
        }

        if( upStreamFE == downStreamChannelFE )
        {
          System.out.println( "impossible net at #" + upStreamFE.getId() );
          continue;
        }
        downStreamElement.addUpStream( upStreamElement );
      }
    }

    // find root elements
    final List rootNetElements = new ArrayList();
    final Feature rootNodeFE = workspace.getFeature( m_conf.getRootNodeId() );
    // fuer root node soll immer ein result generiert werden und fuer das
    // ergebnis auch in eine zml geschrieben werden.
    // falls rootnode angegeben hierfuer auch ein ergebnis generieren
    if( rootNodeFE != null )
    {
      // if rootnode is set, net and results are generated for this
      // root node only
      final FeatureProperty createResultProp = FeatureFactory.createFeatureProperty(
          "generateResult", new Boolean( true ) );
      rootNodeFE.setProperty( createResultProp );
      addRootNode( workspace, rootNodeFE, rootNetElements, netElements, channelFEs );
    }
    else
      // no root node given -> alle results nodes will act as root node
      for( int i = 0; i < nodeFEs.length; i++ )
      {
        Feature nodeFE = nodeFEs[i];
        if( FeatureHelper.booleanIsTrue( nodeFE, "generateResult", false ) )
          addRootNode( workspace, nodeFE, rootNetElements, netElements, channelFEs );
      }

    final List nodeCollector = new ArrayList();

    // generate net file for upstream of root net elements
    //    for( Iterator iter = rootNetElements.iterator(); iter.hasNext(); )
    //    {
    //      NetElement rootElement = (NetElement)iter.next();
    //      rootElement.berechneOberlauf( buffer, nodeCollector );
    //    }
    // generate net for root net elements
    for( Iterator iter = rootNetElements.iterator(); iter.hasNext(); )
    {
      NetElement rootElement = (NetElement)iter.next();
      rootElement.berechne( asciiBuffer, nodeCollector, true );
    }
    // generate net for root channels
    final List processedRootNodes = new ArrayList();
    for( Iterator iter = rootNetElements.iterator(); iter.hasNext(); )
    {
      NetElement rootElement = (NetElement)iter.next();
      List downStreamNetElements = rootElement.getDownStreamNetElements();
      boolean needsDownStreamElement = true;
      for( Iterator iterator = downStreamNetElements.iterator(); iterator.hasNext(); )
      {
        NetElement downStreamElement = (NetElement)iterator.next();
        if( downStreamElement.isCalculated() && !downStreamElement.resultExists() )
          needsDownStreamElement = false;
      }
      if( needsDownStreamElement )
        rootElement.writeRootChannel( asciiBuffer, processedRootNodes );
    }
    asciiBuffer.getNetBuffer().append( "99999\n" );
    appendNodeList( workspace, nodeCollector, asciiBuffer );
    asciiBuffer.getNetBuffer().append( "99999\n" );
  }

  /**
   * adds a feature to the list of root net elements
   */
  private List addRootNode( GMLWorkspace workspace, Feature rootNodeFE, List rootNetElements,
      HashMap netElements, Feature[] channelFEs )
  {
    if( rootNetElements == null )
      rootNetElements = new ArrayList();
    //    rootNodeFE;
    // select netelement from root element
    final Feature rootChannel = workspace.resolveLink( rootNodeFE, "downStreamChannelMember" );
    if( rootChannel != null )
      rootNetElements.add( netElements.get( rootChannel.getId() ) );
    else
    {
      // hat keinen downstream channel
      // finde alle channel die direkt oberhalb sind.
      for( int i = 0; i < channelFEs.length; i++ )
      {
        final Feature channel = channelFEs[i];
        final Feature downStreamNodeFE = workspace.resolveLink( channel, "downStreamNodeMember" );
        if( downStreamNodeFE == rootNodeFE )
          rootNetElements.add( netElements.get( channel.getId() ) );
      }
    }
    // ergebnisse loeschen
    try
    {
      File resultFile = getResultFile( rootNodeFE );
      if( resultFile != null && resultFile.exists() )
        resultFile.delete();
    }
    catch( MalformedURLException e )
    {
      e.printStackTrace();
    }
    return rootNetElements;
  }

  private void appendNodeList( GMLWorkspace workspace, List nodeCollector, AsciiBuffer asciiBuffer )
      throws Exception, Exception
  {
    Iterator iter = nodeCollector.iterator();
    while( iter.hasNext() )
    {
      Feature nodeFE = (Feature)iter.next();

      int izug = 0;
      int iabg = 0;
      int iueb = 0;
      int izuf = 0;
      int ivzwg = 0;
      // verzweigung ?
      Feature linkedNodeFE = workspace.resolveLink( nodeFE, "verzweigungNodeMember" );
      if( linkedNodeFE != null )
        ivzwg = 1;

      // zufluss ?
      final TimeseriesLink zuflussLink;
      final String zuflussFileName;

      if( resultExists( nodeFE ) )
      {
        zuflussLink = (TimeseriesLink)nodeFE.getProperty( "qberechnetZR" );
        zuflussFileName = "result_" + nodeFE.getId();
      }
      else
      {
        zuflussLink = (TimeseriesLink)nodeFE.getProperty( "zuflussZR" );
        zuflussFileName = getZuflussEingabeDateiString( nodeFE );
      }
      if( zuflussLink != null )
        izuf = 5;

      asciiBuffer.getNetBuffer().append(
          FortranFormatHelper.printf( FeatureHelper.getAsString( nodeFE, "num" ), "i5" ) );
      asciiBuffer.getNetBuffer().append( FortranFormatHelper.printf( String.valueOf( izug ), "i5" ) );
      asciiBuffer.getNetBuffer().append( FortranFormatHelper.printf( String.valueOf( iabg ), "i5" ) );
      asciiBuffer.getNetBuffer().append( FortranFormatHelper.printf( String.valueOf( iueb ), "i5" ) );
      asciiBuffer.getNetBuffer().append( FortranFormatHelper.printf( String.valueOf( izuf ), "i5" ) );
      asciiBuffer.getNetBuffer().append(
          FortranFormatHelper.printf( String.valueOf( ivzwg ), "i5" ) + "\n" );

      if( ivzwg != 0 )
      {
        asciiBuffer.getNetBuffer().append(
            FortranFormatHelper.printf( FeatureHelper.getAsString( nodeFE, "zproz" ), "f10.3" ) );
        asciiBuffer.getNetBuffer().append(
            FortranFormatHelper.printf( FeatureHelper.getAsString( linkedNodeFE, "num" ), "i8" ) + "\n" );
      }
      if( izuf != 0 )
      {
        final File targetFile = new File( m_conf.getAsciiBaseDir(), "zufluss/" + zuflussFileName );
        final File parent = targetFile.getParentFile();
        if( !parent.exists() )
          parent.mkdirs();
        String zuflussFile = ZmlURL.getIdentifierPart( zuflussLink.getHref() );
        final URL linkURL = m_urlUtilities.resolveURL( m_conf.getGMLModelURL(), zuflussFile );

        //        final URL linkURL = m_urlUtilities.resolveURL(
        // m_conf.getGMLModelURL(), zuflussLink
        //            .getHref() );

        if( !DEBUG )
        {
          final IObservation observation = ZmlFactory.parseXML( linkURL, "ID" );
          final FileWriter writer = new FileWriter( targetFile );
          NAZMLGenerator.createFile( writer, TimeserieConstants.TYPE_RUNOFF, observation );
          writer.close();
        }
        asciiBuffer.getNetBuffer().append( "    1234\n" ); // dummyLine
        asciiBuffer.getNetBuffer().append(
            ".." + File.separator + "zufluss" + File.separator + zuflussFileName + "\n" );
      }
    }
    // ENDKNOTEN
    asciiBuffer.getNetBuffer().append( "10000    0    0    0    0    0\n" );
  }

  private String getZuflussEingabeDateiString( Feature nodeFE )
  {
    return "Z_" + FeatureHelper.getAsString( nodeFE, "num" ) + ".zufluss";
  }
}