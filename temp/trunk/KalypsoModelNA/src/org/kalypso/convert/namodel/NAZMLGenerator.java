package org.kalypso.convert.namodel;

import java.io.File;
import java.io.FileWriter;
import java.io.Writer;
import java.net.URL;
import java.text.SimpleDateFormat;
import java.util.Date;

import javax.xml.bind.Marshaller;

import org.kalypso.java.net.UrlUtilities;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.ObservationConstants;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.zml.ObservationType;
import org.kalypso.zml.obslink.ObjectFactory;
import org.kalypso.zml.obslink.TimeseriesLink;

/**
 * @author doemming
 *  
 */
public class NAZMLGenerator
{
  private static boolean DEBUG = false;

  // debug = true skips converting ascii timeseries to zml timeseries while importing ascii

  final static SimpleDateFormat m_grapDateFormat = new SimpleDateFormat( "dd MM yyyy HH mm ss" );

  public static final int NA_NIEDERSCHLAG_EINGABE = 1;

  public static final int NA_ZUFLUSS_EINGABE = 2;

  public static final int NA_ABFLUSS_BERECHNET = 3;

  public static final int NA_PEGEL_MESSUNG = 4;
  
  final static NAZMLGenerator m_singelton = new NAZMLGenerator();

  private static final UrlUtilities urlUtilities = new UrlUtilities();


  public NAZMLGenerator()
  {
  // do not instanciate
  }

  /**
   * generate copy of custom timeseriesfile to zml-format, and returns
   * timeserieslink
   * 
   * @param copySource
   *          url to the data to copy
   * @param srcType
   *          constant descibin sourceformat
   * @param targetBaseDir
   *          basedir for targetfile
   * @param targetRelativePath
   *          relative path from basedir to store target zml file
   */
  public static TimeseriesLink copyToTimeseriesLink( URL copySource, int srcType,
      File targetBaseDir, String targetRelativePath, boolean relative, boolean simulateCopy )
      throws Exception
  {
    File targetZmlFile = new File( targetBaseDir, targetRelativePath );
    File dir = targetZmlFile.getParentFile();
    if( !dir.exists() )
      dir.mkdirs();
    if( !simulateCopy && !DEBUG )
      try
      {
        convert( copySource, srcType, targetZmlFile );
      }
      catch( Exception e )
      {
        e.printStackTrace();
        System.out.println( "could not create ZML, but operation will continue..." );
      }
    if( relative )
      return generateobsLink( targetRelativePath, srcType );
    final URL targetURL = urlUtilities.resolveURL( targetBaseDir.toURL(), targetRelativePath );
    return generateobsLink( targetURL.toExternalForm(), srcType );
  }

  /**
   * @param location
   *          location of zml data
   * @param type
   *          constant describing the type of zml, used to generate correct axis
   *          description inside TimeseriesLink
   */
  public static TimeseriesLink generateobsLink( String location, int type ) throws Exception
  {
    // @deprecated old example:
    //    <obslink:TimeseriesLink xmlns:xlink="http://www.w3.org/1999/xlink"
    // linktype="zml" timeaxis="Datum" valueaxis="We
    //      rt" xlink:type="simple" xlink:actuate="onRequest"
    // xlink:href="TYPE=relative#LOCATION=zeitreihen/flutungen/QV_SPWIESE.zml
    //      "/>

    // new example:
    //    <obslink:TimeseriesLink xmlns:xlink="http://www.w3.org/1999/xlink"
    // linktype="zml" timeaxis="Datum" valueaxis="We
    //      rt" xlink:type="simple" xlink:actuate="onRequest"
    // xlink:href="zeitreihen/flutungen/QV_SPWIESE.zml
    //      "/>

    final ObjectFactory factory = new ObjectFactory();

    final TimeseriesLink link = factory.createTimeseriesLink();
    link.setLinktype( "zml" );
    link.setTimeaxis( getAxisName( 1, type ) );
    link.setValueaxis( getAxisName( 2, type ) );
    link.setType( "simple" );
    //    link.setActuate( "onRequest" );
    link.setHref( location );
    //    link.setActuate( "onDemand" );
    return link;
  }

  public static void convert( URL sourceURL, int sourceType, File targetZmlFile ) throws Exception
  {
    StringBuffer buffer = new StringBuffer();
    generateTmpZml( buffer, sourceType, sourceURL );

    File zmlTmpFile = File.createTempFile( "tmp", ".zml" );
    zmlTmpFile.deleteOnExit();
    Writer tmpWriter = new FileWriter( zmlTmpFile );
    tmpWriter.write( buffer.toString() );
    tmpWriter.close();

    IObservation observation = ZmlFactory.parseXML( zmlTmpFile.toURL(), "ID" );
    ObservationType type = ZmlFactory.createXML( observation, null );
    Marshaller marshaller = ZmlFactory.getMarshaller();
    marshaller.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE );
    Writer writer = new FileWriter( targetZmlFile );
    marshaller.marshal( type, writer );
    writer.close();

  }

  private static void generateTmpZml( StringBuffer buffer, int type, URL sourceURL )
      throws Exception
  {
    final String location = sourceURL.toExternalForm();

    buffer.append( "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>" );
    buffer.append( "    <observation xmlns=\"zml.kalypso.org\" " );
    buffer
        .append( " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"zml.kalypso.org./observation.xsd\" xmlns:xlink=\"http://www.w3.org/1999/xlink\">" );
    buffer.append( "  <name>Eine Test-Observation</name>" );
    buffer.append( "      <!-- die Metadaten list ist erweiterbar -->" );
    buffer.append( "      <metadataList>" );
    //    buffer.append( " <metadata name=\"Pegelnullpunkt\" value=\"10\"/>" );
    //    buffer.append( " <metadata name=\"Rechtswert\" value=\"445566\"/>" );
    //    buffer.append( " <metadata name=\"Hochwert\" value=\"887766\"/>" );
    //    buffer.append( " <metadata name=\"Alarmstufe 1\" value=\"4.3\"/>" );
    //    buffer.append( " <metadata name=\"Alarmstufe 2\" value=\"5.6\"/>" );
    buffer.append( "      </metadataList>" );

    //axis1
    buffer.append( "<axis name=\"" + getAxisName( 1, type ) + "\" key=\"true\" type=\""
        + getAxisType( 1, type ) + "\" unit=\"\"" );
    buffer.append( " datatype=\"TYPE=xs:date#FORMAT=dd MM yyyy HH mm ss\">" );
    buffer.append( "<valueLink separator=\",\" column=\"1\" line=\"4\" " );
    buffer.append( " xlink:href=\"" + location + "\"/>" );
    buffer.append( "</axis>" );
    // axis2
    buffer.append( "<axis name=\"" + getAxisName( 2, type ) + "\" " );
    switch( type )
    {
    case NA_NIEDERSCHLAG_EINGABE:
      buffer.append( " type=\"pegel\" unit=\"m\" datatype=\"TYPE=xs:double\">" );
      break;
    case NA_ZUFLUSS_EINGABE:
      buffer.append( " type=\"abfluss\" unit=\"qm/s\" datatype=\"TYPE=xs:double\">" );
      break;
    default:
      break;
    }
    buffer.append( "<valueLink separator=\",\" column=\"2\" line=\"4\" " );
    buffer.append( " xlink:href=\"" + location + "\"/>" );
    buffer.append( "</axis>" );
    buffer.append( "</observation>" );
  }

  private static String getAxisType( int col, int type )
  {
    switch( type )
    {
    case NA_NIEDERSCHLAG_EINGABE:
      return col == 1 ? TimeserieConstants.TYPE_DATE : TimeserieConstants.TYPE_RAINFALL;
    case NA_ZUFLUSS_EINGABE:
      return col == 1 ? TimeserieConstants.TYPE_DATE : TimeserieConstants.TYPE_RUNOFF;
    default:
      break;
    }
    throw new IllegalArgumentException( "unknown file type " + type );
  }

  private static String getAxisName( int col, int type ) throws Exception
  {
    switch( type )
    {
    case NA_NIEDERSCHLAG_EINGABE:
      return col == 1 ? "Datum" : TimeserieConstants.TYPE_RAINFALL;//"Niederschlag";
    case NA_ZUFLUSS_EINGABE:
      return col == 1 ? "Datum" : TimeserieConstants.TYPE_RUNOFF;//"Abfluss";
    case NA_ABFLUSS_BERECHNET:
        return col == 1 ? "Datum" : TimeserieConstants.TYPE_RUNOFF;//"Abfluss";
    case NA_PEGEL_MESSUNG:
        return col == 1 ? "Datum" : TimeserieConstants.TYPE_RUNOFF;//"Abfluss";
    default:
      break;
    }
    throw new IllegalArgumentException( "unknown file type " + type );
  }

  public static void createFile( FileWriter writer, int type, IObservation observation )
      throws Exception
  {
    switch( type )
    {
    case NA_NIEDERSCHLAG_EINGABE:
    case NA_ZUFLUSS_EINGABE:
      createGRAPFile( writer, type, observation );
      break;
    default:
      break;
    }
  }

  private static void createGRAPFile( Writer writer, int type, IObservation observation )
      throws Exception
  {
    // write standard header
    writer.write( "\n" );
    writer.write( "    95090100000  0\n" );
    writer.write( "grap\n" );

    // write data
    IAxis dateAxis = getAxis( observation, getAxisName( 1, type ) );
    IAxis valueAxis = getAxis( observation, getAxisName( 2, type ) );

    ITuppleModel values = observation.getValues( null );
    for( int i = 0; i < values.getCount(); i++ )
    {
      Date date = (Date)values.getElement( i, dateAxis );
      Object value = values.getElement( i, valueAxis );
      writer.write( m_grapDateFormat.format( date ) + " " + value.toString() + "\n" );
    }
  }

  private static IAxis getAxis( IObservation observation, String axisLabel )
  {
    IAxis[] axisList = observation.getAxisList();
    for( int i = 0; i < axisList.length; i++ )
    {
      IAxis axis = axisList[i];
      if( axisLabel.equals( axis.getName() ) )
        return axis;
    }
    return null;
  }
}