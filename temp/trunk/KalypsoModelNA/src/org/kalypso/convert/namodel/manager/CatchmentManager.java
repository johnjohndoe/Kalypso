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
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import org.kalypso.convert.namodel.NAConfiguration;
import org.kalypso.convert.namodel.timeseries.NAZMLGenerator;
import org.kalypso.java.util.FortranFormatHelper;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureProperty;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.gml.schema.GMLSchema;
import org.kalypsodeegree_impl.model.feature.FeatureAssociationTypeProperty_Impl;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

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
    m_grundwasserabflussFT = ( (FeatureAssociationTypeProperty_Impl)ftp2 ).getAssociationFeatureTypes()[0];
  }

  /**
   * 
   * @see org.kalypso.convert.namodel.manager.AbstractManager#parseFile(java.net.URL)
   */
  public Feature[] parseFile( URL url ) throws Exception
  {
    List result = new ArrayList();
    LineNumberReader reader = new LineNumberReader( new InputStreamReader( url.openConnection().getInputStream() ) );// new
                                                                                                                     // FileReader(
                                                                                                                     // file
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
    FeatureProperty bodenkorrekturProperty = FeatureFactory.createFeatureProperty( "bodenkorrekturmember", list );
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
    System.out.println( "12: " + line );
    createProperties( propCollector, line, 12 );

    prop = (FeatureProperty)propCollector.get( "igwzu" );
    // 13-14
    int igwzu = Integer.parseInt( (String)prop.getValue() );
    List gwList = new ArrayList();
    FeatureProperty property = FeatureFactory.createFeatureProperty( "grundwasserabflussMember", gwList );
    propCollector.put( "grundwasserabflussMember", property );
    if( igwzu > 0 )
    {
      HashMap col2 = new HashMap();
      String format13 = FortranFormatHelper.createFormatLine( "ngwzu", "*", "_", igwzu );
      String format14 = FortranFormatHelper.createFormatLine( "gwwi", "*", "_", igwzu );
      line = reader.readLine();
      System.out.println( "13: " + line );
      createProperties( col2, line, format13 );
      line = reader.readLine();
      System.out.println( "14: " + line );
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

    final Feature feature = getFeature( asciiID, m_featureType );

    // handle timeseries: convert to zmllink
    FeatureProperty ts = (FeatureProperty)propCollector.get( "kurzzeit" );
    String tsFileString = (String)ts.getValue();
    String relativeZmlPath = "Niederschlag/Niederschlag_" + feature.getId() + ".zml";
    File orgTsFile = new File( m_conf.getAsciiBaseDir(), "klima.dat/" + tsFileString );
    // repository LINK
    // is absolute
    // do copy
    //TODO: JH, ZML erzeugen, dies funktioniert nicht mehr
    //    Object link = NAZMLGenerator.copyToTimeseriesLink( orgTsFile.toURL(),
    //        TimeserieConstants.TYPE_DATE, TimeserieConstants.TYPE_RAINFALL,
    // m_conf.getGmlBaseDir(),
    //        relativeZmlPath, false, false );
    //    FeatureProperty niederschlagZRRepositoryProp =
    // FeatureFactory.createFeatureProperty(
    //        "niederschlagZRRepository", link );
    //    propCollector.put( "niederschlagZRRrepository",
    // niederschlagZRRepositoryProp );
    // calculation LINK
    // is relative
    // no copy

    Object relativeLink = NAZMLGenerator.copyToTimeseriesLink( orgTsFile.toURL(), TimeserieConstants.TYPE_DATE,
        TimeserieConstants.TYPE_RAINFALL, m_conf.getGmlBaseDir(), relativeZmlPath, true, true );
    FeatureProperty niederschlagZRProp = FeatureFactory.createFeatureProperty( "niederschlagZR", relativeLink );
    propCollector.put( "niederschlagZR", niederschlagZRProp );

    // continue reading

    Collection collection = propCollector.values();
    setParsedProperties( feature, collection );
    line = reader.readLine();
    return feature;
  }

  public void writeFile( AsciiBuffer asciiBuffer, GMLWorkspace workspace ) throws Exception
  {
    Feature rootFeature = workspace.getRootFeature();
    Feature col = (Feature)rootFeature.getProperty( "CatchmentCollectionMember" );
    List list = (List)col.getProperty( "catchmentMember" );
    Iterator iter = list.iterator();
    while( iter.hasNext() )
    {

      final Feature catchmentFE = (Feature)iter.next();
      if( asciiBuffer.writeFeature( catchmentFE ) )
        writeFeature( asciiBuffer, workspace, catchmentFE );
    }
  }

  private void writeFeature( AsciiBuffer asciiBuffer, GMLWorkspace workSpace, Feature feature ) throws Exception
  {
    // 0
    asciiBuffer.getCatchmentBuffer().append(
        "           " + FortranFormatHelper.printf( FeatureHelper.getAsString( feature, "inum" ), "i5" ) + "      7"
            + "\n" );
    // 1-2
    for( int i = 1; i <= 2; i++ )
      asciiBuffer.getCatchmentBuffer().append( toAscci( feature, i ) + "\n" );

    // 3
    StringBuffer b = new StringBuffer();
    //TODO: syntetischen Niederschlag einf�gen in control schema
    //    b.append( FortranFormatHelper.printf( FeatureHelper.getAsString( feature,
    // "pns" ), "a1" ) );
    b.append( "n " + getNiederschlagEingabeDateiString( feature ) );
    b.append( " " + getNiederschlagEingabeDateiString( feature ) );
    b.append( " " + FortranFormatHelper.printf( FeatureHelper.getAsString( feature, "faktn" ), "f5.2" ) + "\n" );

    // 4-6
    b.append( "std.tmp std.ver\n" );
    asciiBuffer.getCatchmentBuffer().append( b.toString() );

    asciiBuffer.getCatchmentBuffer().append( "we999.zfl\n" );
    asciiBuffer.getCatchmentBuffer().append( "we.hyd\n" );

    //7

    asciiBuffer.getCatchmentBuffer().append( toAscci( feature, 7 ) + "\n" );

    //8
    List list = (List)feature.getProperty( "bodenkorrekturmember" );

    StringBuffer buf = new StringBuffer();
    buf.append( FortranFormatHelper.printf( FeatureHelper.getAsString( feature, "vsg" ), "f5.3" ) );
    //anzlayy
    buf.append( FortranFormatHelper.printf( Integer.toString( list.size() ), "i5" ) );
    buf.append( "     " + FortranFormatHelper.printf( FeatureHelper.getAsString( feature, "bimax" ), "f5.1" ) );
    buf.append( "     " + FortranFormatHelper.printf( FeatureHelper.getAsString( feature, "bianf" ), "f5.1" ) );

    Feature nodeFeVers = workSpace.resolveLink( feature, "izkn_vers" );
    if( nodeFeVers == null )
      buf.append( "    0" );
    else
      buf.append( toAscci( nodeFeVers, 18 ) );
    buf.append( "     " + FortranFormatHelper.printf( FeatureHelper.getAsString( feature, "tint" ), "f5.1" ) );
    buf.append( "     " + FortranFormatHelper.printf( FeatureHelper.getAsString( feature, "rintmx" ), "f5.1" ) + "\n" );
    asciiBuffer.getCatchmentBuffer().append( buf.toString() );
    Double banf = (Double)feature.getProperty( "faktorBianf" );

    // 9

    Iterator iter = list.iterator();
    while( iter.hasNext() )
    {
      Feature fe = (Feature)iter.next();
      if( banf != null )
      {
        fe.setProperty( FeatureFactory.createFeatureProperty( "banf", banf ) );
      }
      asciiBuffer.getCatchmentBuffer().append( toAscci( fe, 9 ) + "\n" );
    }
    // 10-11
    for( int i = 10; i <= 11; i++ )
    {
      asciiBuffer.getCatchmentBuffer().append( toAscci( feature, i ) + "\n" );
    }

    // 12-14
    List gwList = (List)feature.getProperty( "grundwasserabflussMember" );
    asciiBuffer.getCatchmentBuffer().append(
        FortranFormatHelper.printf( Integer.toString( gwList.size() ), "*" ) + "\n" );
    StringBuffer line13 = new StringBuffer();
    StringBuffer line14 = new StringBuffer();
    double sumGwwi = 0.0;
    for( Iterator iterator = gwList.iterator(); iterator.hasNext(); )
    {
      Feature fe = (Feature)iterator.next();
      Feature linkedFE = workSpace.resolveLink( fe, "ngwzu" );
      if( linkedFE == null )
        throw new Exception( "Fehler!!! NA-Modell: Grundwasserabfluss in unbekanntes Teilgebiet: #"
            + FeatureHelper.getAsString( fe, "ngwzu" ) );

      line13.append( toAscci( linkedFE, 17 ) + " " );
      line14.append( toAscci( fe, 14 ) + " " );
      sumGwwi += ( (Double)fe.getProperty( "gwwi" ) ).doubleValue();
    }
    if( sumGwwi > 1.0 )
      throw new Exception(
          "Fehler!!! NA-Modell: Summe Grundwasserabgabe in Nachbargebiete > 1.0 (100%) in Teilgebiet: #"
              + FeatureHelper.getAsString( feature, "inum" ) );

    if( gwList.size() > 0 )
    {
      asciiBuffer.getCatchmentBuffer().append( line13 + "\n" );
      asciiBuffer.getCatchmentBuffer().append( line14 + "\n" );
    }
    // 15

    StringBuffer buffer = new StringBuffer();
    buffer.append( FortranFormatHelper.printf( FeatureHelper.getAsString( feature, "hgru" ), "*" ) );
    buffer.append( " " + FortranFormatHelper.printf( FeatureHelper.getAsString( feature, "hgro" ), "*" ) );
    buffer.append( " " + FortranFormatHelper.printf( FeatureHelper.getAsString( feature, "rtr" ), "*" ) );
    buffer.append( " " + FortranFormatHelper.printf( FeatureHelper.getAsString( feature, "pors" ), "*" ) );
    buffer.append( " " + FortranFormatHelper.printf( FeatureHelper.getAsString( feature, "gwsent" ), "*" ) );
    buffer.append( " " + FortranFormatHelper.printf( FeatureHelper.getAsString( feature, "klupor" ), "*" ) );

    Feature nodeFeGW = workSpace.resolveLink( feature, "izkn" );
    if( nodeFeVers == null )
      buffer.append( " 0\n" );
    else
      buffer.append( toAscci( nodeFeGW, 18 ) + "\n" );

    asciiBuffer.getCatchmentBuffer().append( buffer.toString() );

    // KommentarZeile
    asciiBuffer.getCatchmentBuffer().append( "ende gebietsdatensatz" + "\n" );

  } /*
     * (non-Javadoc)
     * 
     * @see org.kalypso.convert.AbstractManager#mapID(int, org.kalypsodeegree.model.feature.FeatureType)
     */

  public String mapID( int id, FeatureType ft )
  {
    return ft.getName() + id;
  }

  public static String getNiederschlagEingabeDateiString( Feature feature )
  {
    return "C_" + FeatureHelper.getAsString( feature, "inum" ) + ".niederschlag";
  }
}