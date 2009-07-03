package org.kalypso.convert.namodel;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.io.Writer;
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
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.zml.obslink.TimeseriesLink;

/**
 * @author doemming
 */
public class NetFileManager extends AbstractManager
{
  static boolean DEBUG = false;//true; //TODO auf false setzen

  final NAConfiguration m_conf;

  private final UrlUtilities m_urlUtilities;

  protected boolean resultExists( Feature nodeFE )
  {
    try
    {
      // rootNode is what we want to calculate, so do not use existing results
      // for
      // this node
      if( m_conf.getRootNodeId().equals( nodeFE.getId() ) )
        return false;
      final TimeseriesLink link = (TimeseriesLink)nodeFE.getProperty( "qberechnetZR" );
      if( link == null )
        return false;
      final String href = link.getHref();
      final URL url = m_urlUtilities.resolveURL( m_conf.getGMLModelURL(), href );
      final File resultFile = new File( url.getFile() );
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

  }

  public String mapID( int id, FeatureType ft )
  {
    return ft.getName() + id;
  }

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
            NAZMLGenerator.NA_ZUFLUSS_EINGABE, m_conf.getGmlBaseDir(), zmlPath, false, false );
        FeatureProperty linkPropertyRepository = FeatureFactory.createFeatureProperty(
            "zuflussZRRepository", link1 );
        propCollector.put( "zuflussZRRepository", linkPropertyRepository );

        TimeseriesLink link2 = NAZMLGenerator.copyToTimeseriesLink( tsFile.toURL(),
            NAZMLGenerator.NA_ZUFLUSS_EINGABE, m_conf.getGmlBaseDir(), zmlPath, true, true );
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
          NAZMLGenerator.NA_ABFLUSS_BERECHNET, m_conf // TODO NA_PEGEL
              .getGmlBaseDir(), "Pegel/Pegel_" + fe.getId() + ".zml", true, true );
      FeatureProperty pegelProp = FeatureFactory.createFeatureProperty( "pegelZR", pegelLink );
      propCollector.put( "pegelZR", pegelProp );

      final TimeseriesLink resultLink = NAZMLGenerator.copyToTimeseriesLink( null,
          NAZMLGenerator.NA_ABFLUSS_BERECHNET, m_conf.getGmlBaseDir(),
          "Ergebnisse/Berechnet/Abfluss_" + fe.getId() + ".zml", true, true );
      FeatureProperty ergProp = FeatureFactory.createFeatureProperty( "qberechnetZR", resultLink );
      propCollector.put( "qberechnetZR", ergProp );

      setParsedProperties( fe, propCollector.values() );
    }
  }

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
   * (non-Javadoc)
   * 
   * @see org.kalypso.convert.namodel.AbstractManager#writeFile(java.io.Writer,
   *      org.deegree.model.feature.GMLWorkspace)
   */
  public void writeFile( Writer writer, GMLWorkspace workspace ) throws Exception
  {
    // 9001
    //
    //

    //    x -> rootNode
    //    |
    //    O -> virtueller Strang generiert NR xxx
    //    |
    //    x -> virtueller knoten generiert NR 10000

    //    final Feature wasterootNodeFE =
    // workspace.getFeature(m_nodeFT,"Node10000");

    // generate virtual Elements

    // generiere abhaengigkeitsstruktur:
    final HashMap netElements = new HashMap();
    // netzelemente generieren
    final List channelList = new ArrayList();
    final Feature[] vChannelFeatures = workspace.getFeatures( m_conf.getVChannelFT() );
    for( int i = 0; i < vChannelFeatures.length; i++ )
      channelList.add( vChannelFeatures[i] );
    final Feature[] kmChannelFeatures = workspace.getFeatures( m_conf.getKmChannelFT() );
    for( int i = 0; i < kmChannelFeatures.length; i++ )
      channelList.add( kmChannelFeatures[i] );

    final Feature[] channelFEs = (Feature[])channelList.toArray( new Feature[channelList.size()] );

    for( int i = 0; i < channelFEs.length; i++ )
      netElements.put( channelFEs[i].getId(), new NetElement( channelFEs[i] ) );
    // abhaengigkeiten hinzufuegen
    //     knoten-knoten
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
      //search upstreamchannel
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
    //     strang-knoten

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
    //     ez -> ez
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
    final Feature rootNodeFE = workspace.getFeature( m_conf.getRootNodeId() );
    final FeatureProperty createResultProp = FeatureFactory.createFeatureProperty(
        "generateResult", new Boolean( true ) );
    // fuer root node soll immer ein result generiert werden und fuer das
    // ergebnis auch in eine zml geschrieben werden.
    rootNodeFE.setProperty( createResultProp );
    //    rootNodeFE;
    // select netelement from root element
    final Feature rootChannel = workspace.resolveLink( rootNodeFE, "downStreamChannelMember" );
    final List rootNetElements = new ArrayList();
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
    //    netElements
    StringBuffer buffer = new StringBuffer();
    final List nodeCollector = new ArrayList();
    for( Iterator iter = rootNetElements.iterator(); iter.hasNext(); )
    {
      NetElement rootElement = (NetElement)iter.next();
      rootElement.berechne( workspace, buffer, nodeCollector ,false);
    }
    buffer.append( "99999\n" );
    appendNodeList( workspace, nodeCollector, buffer );
    buffer.append( "99999\n" );
    writer.write( buffer.toString() );
  }

  private void appendNodeList( GMLWorkspace workspace, List nodeCollector, StringBuffer buffer )
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

      buffer.append( toAscii( FeatureHelper.getAsString( nodeFE, "num" ), "i5" ) );
      buffer.append( toAscii( String.valueOf( izug ), "i5" ) );
      buffer.append( toAscii( String.valueOf( iabg ), "i5" ) );
      buffer.append( toAscii( String.valueOf( iueb ), "i5" ) );
      buffer.append( toAscii( String.valueOf( izuf ), "i5" ) );
      buffer.append( toAscii( String.valueOf( ivzwg ), "i5" ) + "\n" );

      if( ivzwg != 0 )
      {
        buffer.append( toAscii( FeatureHelper.getAsString( nodeFE, "zproz" ), "f10.3" ) );
        buffer.append( toAscii( FeatureHelper.getAsString( linkedNodeFE, "num" ), "i8" ) + "\n" );
      }
      if( izuf != 0 )
      {
        final File targetFile = new File( m_conf.getAsciiBaseDir(), "zufluss/" + zuflussFileName );
        final File parent = targetFile.getParentFile();
        if( !parent.exists() )
          parent.mkdirs();
        final URL linkURL = m_urlUtilities.resolveURL( m_conf.getGMLModelURL(), zuflussLink
            .getHref() );

        if( !DEBUG )
        {
          final IObservation observation = ZmlFactory.parseXML( linkURL, "ID" );
          final FileWriter writer = new FileWriter( targetFile );
          NAZMLGenerator.createFile( writer, NAZMLGenerator.NA_ZUFLUSS_EINGABE, observation );
          writer.close();
        }
        buffer.append( "    1234\n" ); // dummyLine
        buffer.append( ".." + File.separator + "zufluss" + File.separator + zuflussFileName + "\n" );
      }
    }
    // ENDKNOTEN
    buffer.append( "10000    0    0    0    0    0\n" );
  }

  private String getZuflussEingabeDateiString( Feature nodeFE )
  {
    return "Z_" + FeatureHelper.getAsString( nodeFE, "num" ) + ".zufluss";
  }

  public class NetElement
  {
    public final static int UNCALCULATED = 1;

    public final static int CALCULATED = 2;

    private final List m_upStreamDepends = new ArrayList();

    private final List m_downStreamDepends = new ArrayList();

    private final Feature m_channelFE;

    private static final String ANFANGSKNOTEN = "    9001";

    private static final String ENDKNOTEN = "   10000";

    private int m_status = UNCALCULATED;

    private final UrlUtilities m_urlUtils = new UrlUtilities();

    public NetElement( Feature channelFE )
    {
      m_channelFE = channelFE;
    }

    public Feature getChannel()
    {
      return m_channelFE;
    }
/**
 * @param upstream upstream from root node, only first channel is not upstream from rootnode 
 */
    public void berechne( GMLWorkspace workspace, StringBuffer buffer, List nodeList, boolean upstream)
        throws IOException, Exception
    {
      if( m_status == CALCULATED )
        return;

      // falls hier schon ein ergebnis vorliegt wird dieses als zulauf verwendet
      // strangID 9001 notenID
      final Feature knotU = workspace.resolveLink( m_channelFE, "downStreamNodeMember" );
      final boolean resultExists = resultExists( knotU );
      if( !resultExists || !upstream )
        for( Iterator iter = m_upStreamDepends.iterator(); iter.hasNext(); )
        {
          // berechne oberlauf
          NetElement element = (NetElement)iter.next();
          element.berechne( workspace, buffer, nodeList,true );
        }
      // berechne mich
      write( workspace, buffer, nodeList, resultExists,upstream );
      generateTimeSeries( workspace );
      m_status = CALCULATED;
    }

    private void generateTimeSeries( GMLWorkspace workspace ) throws IOException, Exception
    {
      Feature[] catchmentFeatures = workspace.resolveWhoLinksTo( m_channelFE, m_conf
          .getCatchemtFT(), "entwaesserungsStrangMember" );
      for( int i = 0; i < catchmentFeatures.length; i++ )
      {
        final Feature feature = catchmentFeatures[i];
        final TimeseriesLink link = (TimeseriesLink)feature.getProperty( "niederschlagZR" );
        final URL linkURL = m_urlUtils.resolveURL( m_conf.getGMLModelURL(), link.getHref() );
        final String tsFileName = CatchmentManager.getNiederschlagEingabeDateiString( feature );
        final File targetFile = new File( m_conf.getAsciiBaseDir(), "klima.dat/" + tsFileName );
        final File parent = targetFile.getParentFile();
        if( !parent.exists() )
          parent.mkdirs();

        if( !DEBUG )
        {

          final IObservation observation = ZmlFactory.parseXML( linkURL, "ID" );
          final FileWriter writer = new FileWriter( targetFile );
          NAZMLGenerator.createFile( writer, NAZMLGenerator.NA_NIEDERSCHLAG_EINGABE, observation );
          writer.close();
        }
      }
    }

    private void addDownStream( NetElement downStreamElement )
    {
      if( !m_downStreamDepends.contains( downStreamElement ) )
        m_downStreamDepends.add( downStreamElement );
    }

    public void addUpStream( NetElement upStreamElement )
    {
      if( !m_upStreamDepends.contains( upStreamElement ) )
        m_upStreamDepends.add( upStreamElement );
      upStreamElement.addDownStream( this );
    }

    public void write( GMLWorkspace workspace, StringBuffer buffer, List nodeList,
        boolean resultExists,boolean upstream )
    {
      //      System.out.println( "calculate: " + m_channelFE.getId() );
      //   obererknoten:
      buffer.append( toAscci( m_channelFE, 12 ) );

      final Feature knotU = workspace.resolveLink( m_channelFE, "downStreamNodeMember" );

      if( !resultExists || !upstream)
      {
        Feature[] features = workspace.getFeatures( m_conf.getNodeFT() );
        Feature knotO = null;
        for( int i = 0; i < features.length; i++ )
        {
          if( m_channelFE == workspace.resolveLink( features[i], "downStreamChannelMember" ) )
          {
            knotO = features[i];
            continue;
          }
        }
        List catchmentList = new ArrayList();
        Feature[] Cfeatures = workspace.getFeatures( m_conf.getCatchemtFT() );
        for( int i = 0; i < Cfeatures.length; i++ )
        {
          if( m_channelFE == workspace.resolveLink( Cfeatures[i], "entwaesserungsStrangMember" ) )
            catchmentList.add( Cfeatures[i] );
        }
        // oberer knoten
        if( knotO != null )
          buffer.append( toAscci( knotO, 11 ) );
        else
          buffer.append( ANFANGSKNOTEN );

        // unterhalb des rootnodes letzter strang zum endknoten
        // ohne teilgebiete
        if( knotO != null && m_conf.getRootNodeId().equals( knotO.getId() ) )
        {
          buffer.append( ENDKNOTEN );
          buffer.append( " 0\n" ); // no catchments
        }
        else
        {
          // normaler strang, evt. mit teilgebieten
          buffer.append( toAscci( knotU, 11 ) );

          buffer.append( " " + catchmentList.size() + "\n" );
          for( Iterator iter = catchmentList.iterator(); iter.hasNext(); )
          {
            Feature catchmentFE = (Feature)iter.next();
            buffer.append( toAscci( catchmentFE, 12 ) + "\n" );
          }
        }
        if( knotO != null && !nodeList.contains( knotO ) )
          nodeList.add( knotO );
      }
      else
      // result exists
      {
        buffer.append( ANFANGSKNOTEN );
        buffer.append( toAscci( knotU, 11 ) );
        buffer.append( " 0\n" ); // no catchments

      }
      if( !nodeList.contains( knotU ) )
        nodeList.add( knotU );
    }
  }
}