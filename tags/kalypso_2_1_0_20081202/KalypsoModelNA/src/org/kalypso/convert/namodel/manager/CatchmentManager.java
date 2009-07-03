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
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import javax.xml.namespace.QName;

import org.apache.commons.io.IOUtils;
import org.kalypso.contribs.java.util.FortranFormatHelper;
import org.kalypso.convert.namodel.NAConfiguration;
import org.kalypso.convert.namodel.NaModelConstants;
import org.kalypso.convert.namodel.timeseries.NAZMLGenerator;
import org.kalypso.gmlschema.GMLSchema;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * @author doemming
 */
public class CatchmentManager extends AbstractManager
{
  private final IFeatureType m_catchmentFT;

  private final IFeatureType m_bodenKorrekturFT;

  private final IFeatureType m_grundwasserabflussFT;

  private final NAConfiguration m_conf;

  public static final String STD_TEMP_FILENAME = "std.tmp";

  public static final String STD_VERD_FILENAME = "std.ver";

  private static final HashMap<String, String> m_fileMap = new HashMap<String, String>();

  public CatchmentManager( GMLSchema schema, NAConfiguration conf ) throws IOException
  {
    super( conf.getCatchmentFormatURL() );
    m_conf = conf;
    m_catchmentFT = schema.getFeatureType( NaModelConstants.CATCHMENT_ELEMENT_FT );
    final IRelationType ftp1 = (IRelationType) m_catchmentFT.getProperty( NaModelConstants.BODENKORREKTUR_MEMBER );
    m_bodenKorrekturFT = ftp1.getTargetFeatureType();

    final IRelationType ftp2 = (IRelationType) m_catchmentFT.getProperty( NaModelConstants.GRUNDWASSERABFLUSS_MEMBER );
    m_grundwasserabflussFT = ftp2.getTargetFeatureType();
  }

  /**
   * @see org.kalypso.convert.namodel.manager.AbstractManager#parseFile(java.net.URL)
   */
  @Override
  public Feature[] parseFile( URL url ) throws Exception
  {
    List<Feature> result = new ArrayList<Feature>();
    LineNumberReader reader = new LineNumberReader( new InputStreamReader( url.openConnection().getInputStream() ) );// new
    // FileReader(
    // file
    // ) );
    Feature fe = null;
    while( (fe = readNextFeature( reader )) != null )
      result.add( fe );
    return result.toArray( new Feature[result.size()] );
  }

  private Feature readNextFeature( LineNumberReader reader ) throws Exception
  {
    final HashMap<String, String> propCollector = new HashMap<String, String>();
    final Map<IPropertyType, Object> fePropMap = new LinkedHashMap<IPropertyType, Object>();
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
    // FeatureProperty prop = propCollector.get( "anzlayy" );
    final int anzlayy = Integer.parseInt( propCollector.get( "anzlayy" ) );
    final List<Feature> list = new ArrayList<Feature>();
    final IPropertyType pt = m_catchmentFT.getProperty( NaModelConstants.BODENKORREKTUR_MEMBER );
    fePropMap.put( pt, list );
    // 9
    for( int i = 0; i < anzlayy; i++ )
    {
      line = reader.readLine();
      System.out.println( i + ": " + line );
      HashMap<String, String> col2 = new HashMap<String, String>();
      createProperties( col2, line, 9 );
      final Feature bodenkorrekturFE = createFeature( m_bodenKorrekturFT );
      // Collection collection = col2.values();
      setParsedProperties( bodenkorrekturFE, col2, null );
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

    // FeatureProperty prop = propCollector.get( "igwzu" );
    // 13-14
    int igwzu = Integer.parseInt( propCollector.get( "igwzu" ) );
    List<Feature> gwList = new ArrayList<Feature>();

    fePropMap.put( m_catchmentFT.getProperty( NaModelConstants.GRUNDWASSERABFLUSS_MEMBER ), gwList );
    if( igwzu > 0 )
    {
      final HashMap<String, String> col2 = new HashMap<String, String>();
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
        // FeatureProperty fp1 = (FeatureProperty) col2.get( "ngwzu" + i );
        int ngwzuID = Integer.parseInt( col2.get( "ngwzu" + i ) );
        Feature ngwzuFE = getFeature( ngwzuID, m_catchmentFT );
        String ngwzuStringID = ngwzuFE.getId();
        // FeatureProperty ngwzuProp = FeatureFactory.createFeatureProperty( "ngwzu", ngwzuStringID );
        fe.setProperty( NaModelConstants.CATCHMENT_PROP_NGWZU, ngwzuStringID );
        // FeatureProperty fp2 = (FeatureProperty) col2.get( "gwwi" + i );
        // FeatureProperty nwwiProp = FeatureFactory.createFeatureProperty( "gwwi", fp2.getValue() );
        fe.setProperty( NaModelConstants.CATCHMENT_PROP_GWWI, col2.get( "gwwi" + i ) );
        gwList.add( fe );
      }
    }
    // 15
    line = reader.readLine();
    System.out.println( 15 + ": " + line );
    createProperties( propCollector, line, 15 );

    // generate id:
    // prop = propCollector.get( "inum" );
    int asciiID = Integer.parseInt( propCollector.get( "name" ) );

    final Feature feature = getFeature( asciiID, m_catchmentFT );

    // handle timeseries: convert to zmllink
    // FeatureProperty ts = propCollector.get( "kurzzeit" );
    String tsFileString = propCollector.get( "kurzzeit" );
    String relativeZmlPath = "Niederschlag/Niederschlag_" + feature.getId() + ".zml";
    File orgTsFile = new File( m_conf.getAsciiBaseDir(), "klima.dat/" + tsFileString );

    // JH: ZML erzeugen, dies funktioniert nicht mehr

    // Object link = NAZMLGenerator.copyToTimeseriesLink( orgTsFile.toURL(),
    // TimeserieConstants.TYPE_DATE, TimeserieConstants.TYPE_RAINFALL,
    // m_conf.getGmlBaseDir(),
    // relativeZmlPath, false, false );
    // FeatureProperty niederschlagZRRepositoryProp =
    // FeatureFactory.createFeatureProperty(
    // "niederschlagZRRepository", link );
    // propCollector.put( "niederschlagZRRrepository",
    // niederschlagZRRepositoryProp );

    // calculation LINK
    // is relative
    // no copy

    final Object relativeLink = NAZMLGenerator.copyToTimeseriesLink( orgTsFile.toURL(), TimeserieConstants.TYPE_DATE, TimeserieConstants.TYPE_RAINFALL, m_conf.getGmlBaseDir(), relativeZmlPath, true, true );
    fePropMap.put( m_catchmentFT.getProperty( NaModelConstants.CATCHMENT_PROP_ZR_NIEDERSCHLAG ), relativeLink );

    // continue reading

    // Collection collection = propCollector.values();
    setParsedProperties( feature, propCollector, fePropMap );
    line = reader.readLine();
    return feature;
  }

  public void writeFile( AsciiBuffer asciiBuffer, GMLWorkspace workspace ) throws Exception
  {
    Feature rootFeature = workspace.getRootFeature();
    Feature col = (Feature) rootFeature.getProperty( NaModelConstants.CATCHMENT_COLLECTION_MEMBER_PROP );
    List list = (List) col.getProperty( NaModelConstants.CATCHMENT_MEMBER_PROP );
    Iterator iter = list.iterator();
    while( iter.hasNext() )
    {
      final Feature catchmentFE = (Feature) iter.next();
      if( asciiBuffer.writeFeature( catchmentFE ) )
        writeFeature( asciiBuffer, workspace, catchmentFE );
    }
  }

  private void writeFeature( AsciiBuffer asciiBuffer, GMLWorkspace workSpace, final Feature feature ) throws Exception
  {
    // 0
    final IDManager idManager = m_conf.getIdManager();
    int asciiID = idManager.getAsciiID( feature );
    asciiBuffer.getCatchmentBuffer().append( "           " );
    asciiBuffer.getCatchmentBuffer().append( FortranFormatHelper.printf( asciiID, "i5" ) );
    asciiBuffer.getCatchmentBuffer().append( "      7\n" );
    // 1-2
    for( int i = 1; i <= 2; i++ )
      asciiBuffer.getCatchmentBuffer().append( toAscci( feature, i ) + "\n" );

    // 3
    StringBuffer b = new StringBuffer();
    if( m_conf.isUsePrecipitationForm().equals( true ) )
    {
      b.append( "s " );
    }
    else
    {
      b.append( "n " );
    }
    b.append( getNiederschlagEingabeDateiString( feature, m_conf ) );
    b.append( " " + getNiederschlagEingabeDateiString( feature, m_conf ) );
    b.append( " " + FortranFormatHelper.printf( FeatureHelper.getAsString( feature, "faktn" ), "f5.2" ) + "\n" );

    // 4-6
    b.append( getTemperaturEingabeDateiString( feature, m_conf ) );
    b.append( " " );
    b.append( getVerdunstungEingabeDateiString( feature, m_conf ) );
    b.append( "\n" );
    asciiBuffer.getCatchmentBuffer().append( b.toString() );
    // Zeitflächenfunktion
    Object zftProp = feature.getProperty( NaModelConstants.CATCHMENT_PROP_ZFT );
    if( zftProp instanceof IObservation )
    {
      asciiBuffer.getCatchmentBuffer().append( "we_nat.zft\n" );
      writeZML( (IObservation) zftProp, asciiID, asciiBuffer.getZFTBuffer() );
    }
    else
    {
      System.out.println( "Teilgebiet " + asciiID + " wird Standard-Zeitflächenfunktion zugeordnet (im Modell ist dem Teilgebiet keine Zeitflächenfunktion zugeordnet)" );
      asciiBuffer.getCatchmentBuffer().append( "we999.zfl\n" );
    }
    asciiBuffer.getCatchmentBuffer().append( "we.hyd\n" );

    // 7

    asciiBuffer.getCatchmentBuffer().append( toAscci( feature, 7 ) + "\n" );

    // 8
    List list = (List) feature.getProperty( NaModelConstants.BODENKORREKTUR_MEMBER );

    StringBuffer buf = new StringBuffer();
    // Der Versiegelungsgrad vsg wird gesetzt, da er im Rechenkern aus der Hydrotopdatei übernommen wird und somit in
    // der Gebietsdatei uninteressant ist.
    buf.append( "1.000" + FortranFormatHelper.printf( Integer.toString( list.size() ), "i5" ) );
    buf.append( "     " + "  1.0" ); // JH: dummy for bimax, because it is not used in fortran!
    // buf.append( " " + FortranFormatHelper.printf( FeatureHelper.getAsString( feature, "bimax" ), "f5.1" ) );
    buf.append( "     " + FortranFormatHelper.printf( FeatureHelper.getAsString( feature, "bianf" ), "f5.1" ) );

    final IRelationType rt = (IRelationType) feature.getFeatureType().getProperty( NaModelConstants.CATCHMENT_PROP_IZKN_VERS );
    final Feature nodeFeVers = workSpace.resolveLink( feature, rt );
    if( nodeFeVers == null )
      buf.append( "    0" );
    else
      buf.append( FortranFormatHelper.printf( Integer.toString( idManager.getAsciiID( nodeFeVers ) ), "i5" ) );

    // buf.append( toAscci( nodeFeVers, 18 ) );
    buf.append( "     " + FortranFormatHelper.printf( FeatureHelper.getAsString( feature, "tint" ), "f5.1" ) );
    buf.append( "     " + FortranFormatHelper.printf( FeatureHelper.getAsString( feature, "rintmx" ), "f5.1" ) + "\n" );
    asciiBuffer.getCatchmentBuffer().append( buf.toString() );

    // 9 (cinh,*)_(cind,*)_(cex,*)_(bmax,*)_(banf,*)_(fko,*)_(retlay,*)
    // JH: + dummy for "evalay", because the parameter is not used in fortran code
    Iterator iter = list.iterator();
    while( iter.hasNext() )
    {
      Feature fe = (Feature) iter.next();
      asciiBuffer.getCatchmentBuffer().append( toAscci( fe, 9 ) + " 1.0" + "\n" );
    }

    // 10 (____(f_eva,f4.2)_(aint,f3.1)__(aigw,f6.2)____(fint,f4.2)____(ftra,f4.2))
    // JH: only "aigw" from gml. other parameters are not used by fortran program - dummys!
    asciiBuffer.getCatchmentBuffer().append( "1.00 0.0 " + FortranFormatHelper.printf( FeatureHelper.getAsString( feature, "aigw" ), "f6.2" ) + " 0.00 0.00" + "\n" );

    // 11 (retvs,*)_(retob,*)_(retint,*)_(retbas,*)_(retgw,*)_(retklu,*))
    // if correction factors of retention constants are choosen, retention constants correction
    Double faktorRetvs = FeatureHelper.getAsDouble( feature, NaModelConstants.CATCHMENT_PROP_FAKTOR_RETVS, 1 );
    Double faktorRetob = FeatureHelper.getAsDouble( feature, NaModelConstants.CATCHMENT_PROP_FAKTOR_RETOB, 1 );
    Double faktorRetint = FeatureHelper.getAsDouble( feature, NaModelConstants.CATCHMENT_PROP_FAKTOR_RETINT, 1 );
    Double faktorRetbas = FeatureHelper.getAsDouble( feature, NaModelConstants.CATCHMENT_PROP_FAKTOR_RETBAS, 1 );
    Double faktorRetgw = FeatureHelper.getAsDouble( feature, NaModelConstants.CATCHMENT_PROP_FAKTOR_RETGW, 1 );
    Double faktorRetklu = FeatureHelper.getAsDouble( feature, NaModelConstants.CATCHMENT_PROP_FAKTOR_RETKLU, 1 );
    double retvs = faktorRetvs.doubleValue() * ((Double) feature.getProperty( NaModelConstants.CATCHMENT_PROP_RETVS )).doubleValue();
    double retob = faktorRetob.doubleValue() * ((Double) feature.getProperty( NaModelConstants.CATCHMENT_PROP_RETOB )).doubleValue();
    double retint = faktorRetint.doubleValue() * ((Double) feature.getProperty( NaModelConstants.CATCHMENT_PROP_RETINT )).doubleValue();
    double retbas = faktorRetbas.doubleValue() * ((Double) feature.getProperty( NaModelConstants.CATCHMENT_PROP_RETBAS )).doubleValue();
    double retgw = faktorRetgw.doubleValue() * ((Double) feature.getProperty( NaModelConstants.CATCHMENT_PROP_RETGW )).doubleValue();
    double retklu = faktorRetklu.doubleValue() * ((Double) feature.getProperty( NaModelConstants.CATCHMENT_PROP_RETKLU )).doubleValue();

    // asciiBuffer.getCatchmentBuffer().append( toAscci( feature, 11 ) + "\n" );
    asciiBuffer.getCatchmentBuffer().append( FortranFormatHelper.printf( retvs, "*" ) + " " + FortranFormatHelper.printf( retob, "*" ) + " " + FortranFormatHelper.printf( retint, "*" ) + " "
        + FortranFormatHelper.printf( retbas, "*" ) + " " + FortranFormatHelper.printf( retgw, "*" ) + " " + FortranFormatHelper.printf( retklu, "*" ) + "\n" );
    // 12-14
    List gwList = (List) feature.getProperty( NaModelConstants.GRUNDWASSERABFLUSS_MEMBER );
    asciiBuffer.getCatchmentBuffer().append( FortranFormatHelper.printf( Integer.toString( gwList.size() ), "*" ) + "\n" );
    StringBuffer line13 = new StringBuffer();
    StringBuffer line14 = new StringBuffer();
    double sumGwwi = 0.0;
    for( Iterator iterator = gwList.iterator(); iterator.hasNext(); )
    {
      final Feature fe = (Feature) iterator.next();
      final IRelationType rt2 = (IRelationType) fe.getFeatureType().getProperty( NaModelConstants.CATCHMENT_PROP_NGWZU );
      final Feature linkedFE = workSpace.resolveLink( fe, rt2 );

      if( linkedFE == null )
        throw new Exception( "Fehler!!! NA-Modell: Grundwasserabfluss in unbekanntes Teilgebiet: #" + FeatureHelper.getAsString( fe, "ngwzu" ) );
      line13.append( Integer.toString( idManager.getAsciiID( linkedFE ) ).trim() + " " );
      // line13.append( toAscci( linkedFE, 17 ) + " " );
      line14.append( toAscci( fe, 14 ) + " " );
      sumGwwi += ((Double) fe.getProperty( NaModelConstants.CATCHMENT_PROP_GWWI )).doubleValue();
    }

    if( sumGwwi > 1.001 )
      throw new Exception( "Fehler!!! NA-Modell: Summe Grundwasserabgabe in Nachbargebiete > 1.0 (100%) in Teilgebiet (Name: " + feature.getProperty( NaModelConstants.GML_FEATURE_NAME_PROP )
          + ", AsciiID: " + asciiID );
    if( sumGwwi < 0.999 )
    {
      // Restanteil in virtuelles Teilgebiet außerhalb des Einzugsgebietes
      double delta = 1 - sumGwwi;
      line13.append( "0 " );
      line14.append( delta + " " );
      System.out.println( "Achtung!!! Grundwasserabfluss aus Teilgebiet (Name: " + feature.getProperty( NaModelConstants.GML_FEATURE_NAME_PROP ) + ", ID: " + asciiID + ") beträgt nur " + sumGwwi
          * 100 + "%!" );
      System.out.println( "Es müssen 100% abgeschlagen werden! \n Restanteil des Grundwasserabflusses (" + delta * 100 + "%) wird nicht weiter bilanziert. Ist dies gewünscht?\n" );
    }

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
    // tiefengrundwasser
    final IRelationType rt1 = (IRelationType) feature.getFeatureType().getProperty( NaModelConstants.CATCHMENT_PROP_IZKN );
    final Feature nodeFeGW = workSpace.resolveLink( feature, rt1 );

    if( nodeFeGW == null )
      buffer.append( " 0\n" );
    else
      buffer.append( FortranFormatHelper.printf( Integer.toString( idManager.getAsciiID( nodeFeGW ) ), "i5" ) + "\n" );
    // buffer.append( toAscci( nodeFeGW, 18 ) + "\n" );

    asciiBuffer.getCatchmentBuffer().append( buffer.toString() );

    // KommentarZeile
    asciiBuffer.getCatchmentBuffer().append( "ende gebietsdatensatz" + "\n" );

  }

  /**
   * @param observation
   * @param asciiID
   * @param zftBuffer
   * @throws SensorException
   */
  private void writeZML( IObservation observation, int asciiID, StringBuffer zftBuffer ) throws SensorException
  {
    zftBuffer.append( FortranFormatHelper.printf( asciiID, "*" ) + "\n" );

    IAxis[] axisList = observation.getAxisList();
    IAxis hoursAxis = ObservationUtilities.findAxisByType( axisList, TimeserieConstants.TYPE_HOURS );
    IAxis normAreaAxis = ObservationUtilities.findAxisByType( axisList, TimeserieConstants.TYPE_NORM );
    ITuppleModel values = observation.getValues( null );
    int count = values.getCount();
    double t0 = ((Double) values.getElement( 0, hoursAxis )).doubleValue();
    double t1 = ((Double) values.getElement( 1, hoursAxis )).doubleValue();
    double dt = t1 - t0;
    zftBuffer.append( FortranFormatHelper.printf( count, "*" ) + " " + FortranFormatHelper.printf( dt, "*" ) + " 2\n" );
    for( int row = 0; row < count; row++ )
    {
      Double hoursValue = (Double) values.getElement( row, hoursAxis );
      Double normAreaValue = (Double) values.getElement( row, normAreaAxis );
      zftBuffer.append( FortranFormatHelper.printf( hoursValue, "*" ) + " " + FortranFormatHelper.printf( normAreaValue, "*" ) + "\n" );
    }
  }

  /**
   * @see org.kalypso.convert.AbstractManager#mapID(int, org.kalypsodeegree.model.feature.IFeatureType)
   */
  @Override
  public String mapID( int id, IFeatureType ft )
  {
    return ft.getQName().getLocalPart() + id;
  }

  public static String getEingabeDateiString( Feature feature, NAConfiguration conf, String propName, String axisType )
  {
    final String key;
    if( propName.equals( "synthZR" ) )
    {
      key = (String) feature.getProperty( new QName( NaModelConstants.NS_NAMODELL, propName ) );
    }
    else
    {
      final TimeseriesLinkType link = (TimeseriesLinkType) feature.getProperty( new QName( NaModelConstants.NS_NAMODELL, propName ) );
      key = propName + link.getHref();
    }
    if( !m_fileMap.containsKey( key ) )
    {
      final int asciiID = conf.getIdManager().getAsciiID( feature );
      final String name = "C_" + Integer.toString( asciiID ).trim() + "." + axisType;
      m_fileMap.put( key, name );
    }
    return m_fileMap.get( key );
  }

  public static String getNiederschlagEingabeDateiString( Feature feature, NAConfiguration conf )
  {
    if( conf.isUsePrecipitationForm().equals( true ) )
    {
      return getEingabeDateiString( feature, conf, "synthZR", TimeserieConstants.TYPE_RAINFALL );
    }
    return getEingabeDateiString( feature, conf, "niederschlagZR", TimeserieConstants.TYPE_RAINFALL );

  }

  public static String getTemperaturEingabeDateiString( Feature feature, NAConfiguration conf )
  {
    if( feature.getProperty( NaModelConstants.CATCHMENT_PROP_ZR_TEMPERATUR ) != null )
      return getEingabeDateiString( feature, conf, "temperaturZR", TimeserieConstants.TYPE_TEMPERATURE );
    return STD_TEMP_FILENAME;

  }

  /**
   * @param feature
   * @param dir
   */
  public static File getTemperaturEingabeDatei( final Feature feature, final File dir, NAConfiguration conf )
  {
    final String name = getTemperaturEingabeDateiString( feature, conf );
    return new File( dir, name );
  }

  /**
   * @param feature
   */
  public static File getNiederschlagEingabeDatei( final Feature feature, final File dir, NAConfiguration conf )
  {
    return new File( dir, getNiederschlagEingabeDateiString( feature, conf ) );
  }

  public static File getVerdunstungEingabeDatei( Feature feature, File dir, NAConfiguration conf )
  {
    final String name = getVerdunstungEingabeDateiString( feature, conf );
    return new File( dir, name );
  }

  private static String getVerdunstungEingabeDateiString( Feature feature, NAConfiguration conf )
  {
    if( feature.getProperty( NaModelConstants.CATCHMENT_PROP_ZR_VERDUNSTUNG ) != null )
      return getEingabeDateiString( feature, conf, "verdunstungZR", TimeserieConstants.TYPE_EVAPORATION );
    return STD_VERD_FILENAME;

    // int asciiID = conf.getIdManager().getAsciiID( feature );
    // if( feature.getProperty( "verdunstungZR" ) != null )
    // return "C_" + Integer.toString( asciiID ).trim() + ".ver";
  }

  public static void WriteSynthNFile( File targetFileN, Feature feature, GMLWorkspace synthNWorkspace, NAConfiguration conf ) throws Exception
  {
    final List<Feature> statNList = new ArrayList<Feature>();
    StringBuffer buffer = new StringBuffer();
    Double annualityKey = conf.getAnnuality();
    // Kostra-Kachel/ synth. N gebietsabhängig
    String synthNKey = (String) feature.getProperty( NaModelConstants.CATCHMENT_PROP_ZR_SYNTH );
    statNList.addAll( Arrays.asList( synthNWorkspace.getFeatures( conf.getstatNFT() ) ) );
    final Iterator<Feature> iter = statNList.iterator();
    while( iter.hasNext() )
    {
      final Feature statNFE = iter.next();
      if( statNFE.getProperty( NaModelConstants.GML_FEATURE_NAME_PROP ) != null )
      {
        if( ((statNFE.getProperty( NaModelConstants.GML_FEATURE_NAME_PROP )).toString()).equals( synthNKey ) )
        {
          List statNParameterList = (List) statNFE.getProperty( NaModelConstants.STATNPARA_MEMBER );
          Iterator iter1 = statNParameterList.iterator();
          while( iter1.hasNext() )
          {
            final Feature fe = (Feature) iter1.next();
            String annuality = Double.toString( 1d / (Double) fe.getProperty( NaModelConstants.STATN_PROP_XJAH ) );
            if( annuality.equals( annualityKey.toString() ) )
            {
              Object tnProp = fe.getProperty( NaModelConstants.CATCHMENT_PROP_STATN_DIAG );
              if( tnProp instanceof IObservation )
              {
                IObservation observation = (IObservation) tnProp;
                IAxis[] axisList = observation.getAxisList();
                IAxis minutesAxis = ObservationUtilities.findAxisByType( axisList, TimeserieConstants.TYPE_MIN );
                IAxis precipitationAxis = ObservationUtilities.findAxisByType( axisList, TimeserieConstants.TYPE_RAINFALL );
                buffer.append( FortranFormatHelper.printf( annualityKey, "f6.3" ) + " " + "1" + "\n" );
                ITuppleModel values = observation.getValues( null );
                int count = values.getCount();
                // if( count > 20 )
                // throw new Exception( "Fehler!!! NA-Modell: Anzahl Wertepaare synth Niederschlag > maximale Anzahl
                // (20) \n Niederschlag:" + synthNKey + "\n Wiederkehrwahrscheinlichkeit: "
                // + annualityKey );
                for( int row = 0; row < count; row++ )
                {
                  Double minutesValue = (Double) values.getElement( row, minutesAxis );
                  Double hoursValue = minutesValue / 60d;
                  if( hoursValue.equals( conf.getDuration() ) )
                  {
                    Double precipitationValue = (Double) values.getElement( row, precipitationAxis );
                    buffer.append( FortranFormatHelper.printf( hoursValue, "f9.3" ) + " " + FortranFormatHelper.printf( precipitationValue, "*" ) + "\n" );
                  }
                }
                final FileWriter writer = new FileWriter( targetFileN );
                writer.write( buffer.toString() );
                IOUtils.closeQuietly( writer );
              }
              else
                System.out.println( "Es existiert kein synthetischer Niederschlag für : " + synthNKey + ", Wiederkehrintervall: " + annualityKey );
            }
          }
        }
      }
    }

  }
}