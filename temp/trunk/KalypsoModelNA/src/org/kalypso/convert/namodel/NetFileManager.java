package org.kalypso.convert.namodel;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.io.Writer;
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
import org.deegree_impl.gml.schema.GMLSchema;
import org.deegree_impl.model.feature.FeatureFactory;

/**
 * @author doemming
 */
public class NetFileManager extends AbstractManager
{

  private final FeatureType m_nodeFT;

  private final FeatureType m_vChannelFT;

  private final FeatureType m_kmChannelFT;

  private final FeatureType m_catchmentFT;

  public NetFileManager( GMLSchema schema, NAConfiguration conf ) throws IOException
  {
    super( conf.getNetFormatURL() );
    m_nodeFT = schema.getFeatureType( "Node" );
    m_vChannelFT = schema.getFeatureType( "VirtualChannel" );
    m_kmChannelFT = schema.getFeatureType( "KMChannel" );
    m_catchmentFT = schema.getFeatureType( "Catchment" );
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
      final Feature fe = getFeature( knot, m_nodeFT );
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
      }
      if( ivzwg > 0 ) // VERZWEIGUNG
      {
        line = reader.readLine();
        System.out.println( 8 + ": " + line );
        createProperties( propCollector, line, 10 );// zproz ikz
        // resolve targetnode
        FeatureProperty ikzProp = (FeatureProperty)propCollector.get( "ikz" );
        int ikz = Integer.parseInt( (String)ikzProp.getValue() );
        Feature targetNodeFE = getFeature( ikz, m_nodeFT );
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
      setParsedProperties( fe, propCollector.values() );
      System.out.println( "debug" );
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
    final Feature knotoFE = getFeature( iknotoNr, m_nodeFT );
    nodeCollector.put( knotoFE.getId(), knotoFE );
    knotoFE.setProperty( numPropertyKnotO );
    final FeatureProperty numPropertyKnotU = FeatureFactory.createFeatureProperty( "num", ""
        + iknotuNr );
    final Feature knotuFE = getFeature( iknotuNr, m_nodeFT );
    nodeCollector.put( knotuFE.getId(), knotuFE );
    knotuFE.setProperty( numPropertyKnotU );
    // set node channel relations
    final Feature strangFE = getExistingFeature( istrngNr, new FeatureType[]
    {
        m_kmChannelFT,
        m_vChannelFT } );
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
        final Feature teilgebFE = getFeature( nteil, m_catchmentFT );
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
    final Feature[] vChannelFeatures = workspace.getFeatures( m_vChannelFT );
    for( int i = 0; i < vChannelFeatures.length; i++ )
      channelList.add( vChannelFeatures[i] );
    final Feature[] kmChannelFeatures = workspace.getFeatures( m_kmChannelFT );
    for( int i = 0; i < kmChannelFeatures.length; i++ )
      channelList.add( kmChannelFeatures[i] );

    final Feature[] channelFEs = (Feature[])channelList.toArray( new Feature[channelList.size()] );

    for( int i = 0; i < channelFEs.length; i++ )
      netElements.put( channelFEs[i].getId(), new NetElement( channelFEs[i] ) );
    // abhaengigkeiten hinzufuegen
    //     knoten-knoten
    final Feature[] nodeFEs = workspace.getFeatures( m_nodeFT );
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
    Feature[] catchmentFEs = workspace.getFeatures( m_catchmentFT );
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
    final Feature rootNodeFE = workspace.getFeature( m_nodeFT, "Node1000" );
    // select netelement from root element
    Feature rootChannel = workspace.resolveLink( rootNodeFE, "downStreamChannelMember" );
    NetElement rootElement = (NetElement)netElements.get( rootChannel.getId() );
    //    netElements
    rootElement.berechne( rootChannel );
  }

  public class NetElement
  {
    public final static int UNCALCULATED = 1;

    public final static int CALCULATED = 2;

    private final List m_upStreamDepends = new ArrayList();

    private final List m_downStreamDepends = new ArrayList();

    private final Feature m_channelFE;

    public NetElement( Feature channelFE )
    {
      m_channelFE = channelFE;
    }

    public void berechne( Feature parentFE )
    {
//      System.out.println( "\nworking..." + parentFE.getId() + " -> " + m_channelFE.getId() );
      // berechne upstream
      for( Iterator iter = m_upStreamDepends.iterator(); iter.hasNext(); )
      {
        NetElement element = (NetElement)iter.next();
        element.berechne( m_channelFE );
      }
      // berechne mich
      write();
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

    public void write()
    {
      System.out.println( "calculate: " + m_channelFE.getId() );
    }
  }
}