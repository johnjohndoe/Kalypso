package org.kalypso.convert.namodel;

import java.io.File;
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

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureProperty;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree.model.feature.GMLWorkspace;
import org.deegree_impl.gml.schema.GMLSchema;
import org.deegree_impl.model.feature.FeatureAssociationTypeProperty_Impl;
import org.deegree_impl.model.feature.FeatureFactory;

/**
 * @author doemming
 */
public class CatchmentManager extends AbstractManager
{
  private final FeatureType m_featureType;

  private final FeatureType m_bodenKorrekturFT;

  private final FeatureType m_grundwasserabflussFT;

  private final NAConfiguration m_conf;

  public CatchmentManager( GMLSchema schema, NAConfiguration conf ) throws IOException
  {
    super( conf.getCatchmentFormatURL() );
    m_conf = conf;
    m_featureType = schema.getFeatureType( "Catchment" );
    FeatureTypeProperty ftp1 = m_featureType.getProperty( "bodenkorrekturmember" );
    m_bodenKorrekturFT = ( (FeatureAssociationTypeProperty_Impl)ftp1 ).getAssociationFeatureTypes()[0];

    FeatureTypeProperty ftp2 = m_featureType.getProperty( "grundwasserabflussMember" );
    m_grundwasserabflussFT = ( (FeatureAssociationTypeProperty_Impl)ftp2 )
        .getAssociationFeatureTypes()[0];
  }

  /**
   * 
   * @see org.kalypso.convert.namodel.AbstractManager#parseFile(java.net.URL)
   */
  public Feature[] parseFile( URL url ) throws Exception
  {
    List result = new ArrayList();
    LineNumberReader reader = new LineNumberReader( new InputStreamReader( url.openConnection()
        .getInputStream() ) );// new FileReader( file
    // ) );
    Feature fe = null;
    while( ( fe = readNextFeature( reader ) ) != null )
      result.add( fe );
    return (Feature[])result.toArray( new Feature[result.size()] );
  }

  private Feature readNextFeature( LineNumberReader reader ) throws Exception
  {
    HashMap propCollector = new HashMap();
    String line;
    // 0-8
    for( int i = 0; i <= 8; i++ )
    {
      line = reader.readLine();
      if( line == null )
        return null;
      System.out.println( i + ": " + line );
      createProperties( propCollector, line, i );
    }
    FeatureProperty prop = (FeatureProperty)propCollector.get( "anzlayy" );
    int anzlayy = Integer.parseInt( (String)prop.getValue() );
    List list = new ArrayList();
    FeatureProperty bodenkorrekturProperty = FeatureFactory.createFeatureProperty(
        "bodenkorrekturmember", list );
    propCollector.put( bodenkorrekturProperty.getName(), bodenkorrekturProperty );
    // 9
    for( int i = 0; i < anzlayy; i++ )
    {
      line = reader.readLine();
      System.out.println( i + ": " + line );
      HashMap col2 = new HashMap();
      createProperties( col2, line, 9 );
      final Feature bodenkorrekturFE = createFeature( m_bodenKorrekturFT );
      Collection collection = col2.values();
      setParsedProperties( bodenkorrekturFE, collection );
      list.add( bodenkorrekturFE );
    }
    // 10-11
    for( int i = 10; i <= 11; i++ )
    {
      line = reader.readLine();
      System.out.println( i + ": " + line );
      createProperties( propCollector, line, i );
    }
    // 12
    line = reader.readLine();
    createProperties( propCollector, line, 12 );

    prop = (FeatureProperty)propCollector.get( "igwzu" );
    // 13-14
    int igwzu = Integer.parseInt( (String)prop.getValue() );
    List gwList = new ArrayList();
    FeatureProperty property = FeatureFactory.createFeatureProperty( "grundwasserabflussMember",
        gwList );
    propCollector.put( "grundwasserabflussMember", property );
    if( igwzu > 0 )
    {
      HashMap col2 = new HashMap();
      String format13 = createFormatLine( "ngwzu", "*", "_", igwzu );
      String format14 = createFormatLine( "gwwi", "*", "_", igwzu );
      line = reader.readLine();
      createProperties( col2, line, format13 );
      line = reader.readLine();
      createProperties( col2, line, format14 );
      for( int i = 0; i < igwzu; i++ )
      {
        Feature fe = createFeature( m_grundwasserabflussFT );
        FeatureProperty fp1 = (FeatureProperty)col2.get( "ngwzu" + i );
        int ngwzuID = Integer.parseInt( fp1.getValue().toString() );
        Feature ngwzuFE = getFeature( ngwzuID, m_featureType );
        String ngwzuStringID = ngwzuFE.getId();
        FeatureProperty ngwzuProp = FeatureFactory.createFeatureProperty( "ngwzu", ngwzuStringID );
        fe.setProperty( ngwzuProp );
        FeatureProperty fp2 = (FeatureProperty)col2.get( "gwwi" + i );
        FeatureProperty nwwiProp = FeatureFactory.createFeatureProperty( "gwwi", fp2.getValue() );
        fe.setProperty( nwwiProp );
        gwList.add( fe );
      }
    }
    // 15
    line = reader.readLine();
    System.out.println( 15 + ": " + line );
    createProperties( propCollector, line, 15 );

    // generate id:
    prop = (FeatureProperty)propCollector.get( "inum" );
    int asciiID = Integer.parseInt( (String)prop.getValue() );

    // handle timeseries: convert to zmllink
    FeatureProperty ts = (FeatureProperty)propCollector.get( "kurzzeit" );
    String tsFileString = (String)ts.getValue();
    String relativeZmlPath="zml/N_C_"+asciiID+".zml";
    File orgTsFile = new File( m_conf.getAsciiBaseDir(),"klima.dat/"+tsFileString );
    // repository LINK 
    // is absolute
    // do copy
    Object link = NAZMLGenerator
        .copyToTimeseriesLink( orgTsFile.toURL(), NAZMLGenerator.NA_NIEDERSCHLAG_EINGABE, m_conf
            .getGmlBaseDir(),relativeZmlPath, false ,false);
    FeatureProperty niederschlagZRRepositoryProp = FeatureFactory.createFeatureProperty( "niederschlagZRRepository",
        link );
    propCollector.put( "niederschlagZRRrepository", niederschlagZRRepositoryProp );
    // calculation LINK 
    // is relative
    // no copy

    Object relativeLink = NAZMLGenerator
    .copyToTimeseriesLink( orgTsFile.toURL(), NAZMLGenerator.NA_NIEDERSCHLAG_EINGABE, m_conf
        .getGmlBaseDir(),relativeZmlPath, true ,true);
    FeatureProperty niederschlagZRProp = FeatureFactory.createFeatureProperty( "niederschlagZR",
        relativeLink );
    propCollector.put( "niederschlagZR", niederschlagZRProp );

    // continue reading

    
    Feature feature = getFeature( asciiID, m_featureType );
    Collection collection = propCollector.values();
    setParsedProperties( feature, collection );
    line = reader.readLine();
    return feature;
  }

  public void writeFile( Writer writer, GMLWorkspace workspace ) throws Exception
  {
    Feature rootFeature = workspace.getRootFeature();
    Feature col = (Feature)rootFeature.getProperty( "CatchmentCollectionMember" );
    List list = (List)col.getProperty( "catchmentMember" );
    Iterator iter = list.iterator();
    while( iter.hasNext() )
      writeFeature( writer, workspace, (Feature)iter.next() );

    //    ExtFeature[] features = getAllFeatures();
    //    for( int i = 0; i < features.length; i++ )
    //    {
    //      writeFeature( features[i] );
    //    }
  }

  private void writeFeature( Writer writer, GMLWorkspace workSpace, Feature feature )
      throws Exception
  {
    // 0-2
    for( int i = 0; i <= 2; i++ )
      writer.write( toAscci( feature, i ) + "\n" );

    StringBuffer b = new StringBuffer();
    b.append( toAscii( (String)feature.getProperty( "pns" ), "a1" ) );
    b.append( " " + getNiederschlagEingabeDateiString( feature ) );
    b.append( " " + getNiederschlagEingabeDateiString( feature ) );
    b.append( " " + toAscii( (String)feature.getProperty( "faktn" ), "f5.2" ) + "\n" );
    // 5
    b.append("std.tmp std.ver\n");
    writer.write( b.toString() );
    
    // 5-8
    for( int i = 5; i <= 8; i++ )
      writer.write( toAscci( feature, i ) + "\n" );

    // 9
    List list = (List)feature.getProperty( "bodenkorrekturmember" );
    Iterator iter = list.iterator();
    while( iter.hasNext() )
    {
      Feature fe = (Feature)iter.next();

      writer.write( toAscci( fe, 9 ) + "\n" );
    }
    // 10-12
    for( int i = 10; i <= 12; i++ )
    {
      writer.write( toAscci( feature, i ) + "\n" );
    }
    // 13-14
    List gwList = (List)feature.getProperty( "grundwasserabflussMember" );
    StringBuffer line13 = new StringBuffer();
    StringBuffer line14 = new StringBuffer();
    for( Iterator iterator = gwList.iterator(); iterator.hasNext(); )
    {
      Feature fe = (Feature)iterator.next();
      Feature linkedFE = workSpace.resolveLink( fe, "ngwzu" );
      if( linkedFE == null )
        throw new Exception( "broken NA-Modell: grundwasserabfluss in unbekanntes Teilgebiet: #"
            + fe.getProperty( "ngwzu" ).toString() );
      if( linkedFE == null )
        line13.append( "\nbroken NA-Modell: grundwasserabfluss in unbekanntes Teilgebiet: #"
            + fe.getProperty( "ngwzu" ).toString() );
      else
      {

        //      line13.append( toAscci( fe, 13 ) );
        line13.append( toAscci( linkedFE, 17 ) );
        line14.append( toAscci( fe, 14 ) );
      }

    }
    writer.write( line13 + "\n" );
    writer.write( line14 + "\n" );
    // 15

    writer.write( toAscci( feature, 15 ) + "\n" );
    // kommentarZeile
    writer.write( "ende gebietsdatensatz" + "\n" );

  }

  /*
   * (non-Javadoc)
   * 
   * @see org.kalypso.convert.AbstractManager#mapID(int,
   *      org.deegree.model.feature.FeatureType)
   */
  public String mapID( int id, FeatureType ft )
  {
    return ft.getName() + id;
  }

  public static String getNiederschlagEingabeDateiString( Feature feature )
  {
    return "C_" + feature.getProperty( "inum" ) + ".niederschlag";
  }
}