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
package org.kalypso.convert.namodel.timeseries;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.net.URL;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;
import java.util.SortedMap;
import java.util.TreeMap;

import javax.xml.bind.Marshaller;

import org.kalypso.contribs.java.net.UrlUtilities;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.zml.Observation;
import org.kalypso.zml.obslink.ObjectFactory;
import org.kalypso.zml.obslink.TimeseriesLinkType;

/**
 * @author doemming
 */
public class NAZMLGenerator
{
  private static final ObjectFactory OF = new ObjectFactory();

  /**
   * debug = true skips converting ascii timeseries to zml timeseries while importing ascii
   */
  private static boolean DEBUG = false;

  final static DateFormat m_grapDateFormat = NATimeSettings.getInstance().getTimeZonedDateFormat( new SimpleDateFormat( "dd MM yyyy HH mm ss" ) );

  // final static SimpleDateFormat m_grapDateFormat = new SimpleDateFormat( "dd MM yyyy HH mm ss" );

  final static NAZMLGenerator m_singelton = new NAZMLGenerator();

  public NAZMLGenerator( )
  {
    // do not instanciate
  }

  /**
   * generate copy of custom timeseriesfile to zml-format, and returns timeserieslink
   * 
   * @param copySource
   *            url to the data to copy
   * @param targetBaseDir
   *            basedir for targetfile
   * @param targetRelativePath
   *            relative path from basedir to store target zml file
   */
  public static TimeseriesLinkType copyToTimeseriesLink( URL copySource, String axis1Type, String axis2Type, File targetBaseDir, String targetRelativePath, boolean relative, boolean simulateCopy ) throws Exception
  {

    File targetZmlFile = new File( targetBaseDir, targetRelativePath );
    File dir = targetZmlFile.getParentFile();
    if( !dir.exists() )
      dir.mkdirs();
    if( !simulateCopy && !DEBUG )
      try
      {
        convert( copySource, axis1Type, axis2Type, targetZmlFile );
      }
      catch( Exception e )
      {
        e.printStackTrace();
        System.out.println( "could not create ZML, but operation will continue..." );
      }
    if( relative )
      return generateobsLink( targetRelativePath );
    UrlUtilities urlUtilities = new UrlUtilities();
    final URL targetURL = urlUtilities.resolveURL( targetBaseDir.toURL(), targetRelativePath );
    return generateobsLink( targetURL.toExternalForm() );
  }

  /**
   * @param location
   *            location of zml data
   */
  public static TimeseriesLinkType generateobsLink( String location ) throws Exception
  {
    final TimeseriesLinkType link = OF.createTimeseriesLinkType();
    link.setLinktype( "zml" );
    link.setType( "simple" );
    link.setHref( location );
    return link;
  }

  private static void convert( URL sourceURL, String axis1Type, String axis2Type, File targetZmlFile ) throws Exception
  {
    StringBuffer buffer = new StringBuffer();
    generateTmpZml( buffer, axis1Type, axis2Type, sourceURL );

    File zmlTmpFile = File.createTempFile( "tmp", ".zml" );
    zmlTmpFile.deleteOnExit();
    Writer tmpWriter = new FileWriter( zmlTmpFile );
    tmpWriter.write( buffer.toString() );
    tmpWriter.close();

    IObservation observation = ZmlFactory.parseXML( zmlTmpFile.toURL(), "ID" );
    final Observation type = ZmlFactory.createXML( observation, null );
    Marshaller marshaller = ZmlFactory.getMarshaller();
    marshaller.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE );
    Writer writer = new FileWriter( targetZmlFile );
    marshaller.marshal( type, writer );
    writer.close();

  }

  private static void generateTmpZml( StringBuffer buffer, String axis1Type, String axis2Type, URL sourceURL ) throws Exception
  {
    final String location = sourceURL.toExternalForm();

    buffer.append( "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>" );
    buffer.append( "    <observation xmlns=\"zml.kalypso.org\" " );
    buffer.append( " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"zml.kalypso.org./observation.xsd\" xmlns:xlink=\"http://www.w3.org/1999/xlink\">" );
    buffer.append( "  <name>Eine Test-Observation</name>" );
    buffer.append( "      <!-- die Metadaten list ist erweiterbar -->" );
    buffer.append( "      <metadataList>" );
    // buffer.append( " <metadata name=\"Pegelnullpunkt\" value=\"10\"/>" );
    // buffer.append( " <metadata name=\"Rechtswert\" value=\"445566\"/>" );
    // buffer.append( " <metadata name=\"Hochwert\" value=\"887766\"/>" );
    // buffer.append( " <metadata name=\"Alarmstufe 1\" value=\"4.3\"/>" );
    // buffer.append( " <metadata name=\"Alarmstufe 2\" value=\"5.6\"/>" );
    buffer.append( "      </metadataList>" );

    // axis1
    buffer.append( "<axis name=\"" + TimeserieUtils.getName( axis1Type ) + "\" "
    // +"key=\"true\""
        + " type=\"" + axis1Type + "\" unit=\"" + TimeserieUtils.getUnit( axis1Type ) + "\"" );

    // buffer.append( " datatype=\"TYPE=xs:date#FORMAT=dd MM yyyy HH mm ss\"");

    buffer.append( " >" );
    buffer.append( "<valueLink separator=\",\" column=\"1\" line=\"4\" " );
    buffer.append( " xlink:href=\"" + location + "\"/>" );
    buffer.append( "</axis>" );
    // axis2
    buffer.append( "<axis name=\"" + TimeserieUtils.getName( axis2Type ) + "\" "
    // +"key=\"true\""
        + " type=\"" + axis2Type + "\" unit=\"" + TimeserieUtils.getUnit( axis2Type ) + "\"" );

    buffer.append( "<valueLink separator=\",\" column=\"2\" line=\"4\" " );
    buffer.append( " xlink:href=\"" + location + "\"/>" );
    buffer.append( "</axis>" );
    buffer.append( "</observation>" );
  }

  public static void createFile( FileWriter writer, String axisValueType, IObservation observation ) throws Exception
  {
    createGRAPFile( writer, axisValueType, observation );
  }

  public static void createExt2File( final FileWriter writer, final IObservation observation, final Date start, final Date end, final String axisType, final String defaultValue ) throws IOException, SensorException
  {
    final Ext2Writer extWriter = new Ext2Writer( start, end );
    extWriter.write( observation, axisType, writer, defaultValue );
  }

  private static void createGRAPFile( Writer writer, String valueAxisType, IObservation observation ) throws Exception
  {
    final DateFormat dateFormat = NATimeSettings.getInstance().getTimeZonedDateFormat( new SimpleDateFormat( "yyyyMMddHHmm" ) );

    // write standard header

    // write data

    final IAxis[] axis = observation.getAxisList();
    final IAxis dateAxis = ObservationUtilities.findAxisByType( axis, TimeserieConstants.TYPE_DATE );
    final IAxis valueAxis = ObservationUtilities.findAxisByType( axis, valueAxisType );

    final ITuppleModel values = observation.getValues( null );
    for( int i = 0; i < values.getCount(); i++ )
    {
      final Date date = (Date) values.getElement( i, dateAxis );
      final Double value = (Double) values.getElement( i, valueAxis );

      if( i == 0 )
      {
        writer.write( "\n" );
        writer.write( "       " + dateFormat.format( date ) + "\n" );
        writer.write( "grap\n" );

      }
      // sometimes values < 0 are used to indicate measure-failures,
      // unfortunately
      // the simulation kernel will hang in an endless loop and write a endless
      // error file if this occurs, so we better prevent this in any case
      // TODO: JH Do we need a user information on this???

      if( value.doubleValue() < 0 )
        writer.write( m_grapDateFormat.format( date ) + " 0.0\n" );
      else
        writer.write( m_grapDateFormat.format( date ) + " " + value.toString() + "\n" );
    }
  }

  public static void createSyntheticFile( final Writer writer, final String valueAxisType, final IObservation observation, final Date simulationStart, final Date simulationEnd, final int minutesOfTimeStep ) throws Exception
  {
    if( simulationStart.after( simulationEnd ) )
      return;
    final DateFormat dateFormat = NATimeSettings.getInstance().getTimeZonedDateFormat( new SimpleDateFormat( "yyyyMMddHHmm" ) );
    final IAxis[] axis = observation.getAxisList();
    final IAxis valueAxis = ObservationUtilities.findAxisByType( axis, valueAxisType );

    final ITuppleModel values = observation.getValues( null );
    final SortedMap<Date, Double> valuesMap = new TreeMap<Date, Double>();
    final long interpolationStep = (simulationEnd.getTime() - simulationStart.getTime()) / (values.getCount() - 1);
    for( int i = 0; i < values.getCount(); i++ )
    {
      final Date date = new Date( simulationStart.getTime() + i * interpolationStep );
      final Double value = (Double) values.getElement( i, valueAxis );
      valuesMap.put( date, value );
    }
    final Interpolator interpolator = new Interpolator( valuesMap );
    final long syntheticTimeseriesStep = minutesOfTimeStep * 60000; // milliseconds

    Date currentDate = simulationStart;
    writer.write( "\n" );
    writer.write( "       " + dateFormat.format( currentDate ) + "\n" );
    writer.write( "grap\n" );
    do
    {
      final double value = interpolator.getValue( currentDate );
      if( value < 0.0 )
        writer.write( m_grapDateFormat.format( currentDate ) + " 0.0\n" );
      else
        writer.write( m_grapDateFormat.format( currentDate ) + " " + String.format( Locale.US, "%5.3f", value ) + "\n" );
      currentDate = new Date( currentDate.getTime() + syntheticTimeseriesStep );
    }
    while( currentDate.before( simulationEnd ) );
  }
}