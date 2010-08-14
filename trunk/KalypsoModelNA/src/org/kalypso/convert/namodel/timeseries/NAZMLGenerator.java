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
import java.io.IOException;
import java.io.PrintWriter;
import java.net.URL;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.SortedMap;
import java.util.TreeMap;

import org.kalypso.contribs.java.lang.DoubleToString;
import org.kalypso.contribs.java.net.UrlUtilities;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITupleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;
import org.kalypso.zml.obslink.ObjectFactory;
import org.kalypso.zml.obslink.TimeseriesLinkType;

/**
 * @author doemming
 */
public final class NAZMLGenerator
{
  private static final ObjectFactory OF = new ObjectFactory();

  private static final DateFormat DF_GRAP_HEADER = NATimeSettings.getInstance().getTimeZonedDateFormat( new SimpleDateFormat( "yyyyMMddHHmm" ) ); //$NON-NLS-1$

  /**
   * debug = true skips converting ascii timeseries to zml timeseries while importing ascii
   */
  private static boolean DEBUG = false;

  private final static DateFormat GRAP_DATE_FORMAT = NATimeSettings.getInstance().getTimeZonedDateFormat( new SimpleDateFormat( "dd MM yyyy HH mm ss" ) ); //$NON-NLS-1$

  private NAZMLGenerator( )
  {
    // do not instanciate
  }

  /**
   * FIXME: this method is just nonsense. Check what really was intended.<br/>
   * generate copy of custom timeseriesfile to zml-format, and returns timeserieslink
   * 
   * @param copySource
   *          url to the data to copy
   * @param targetBaseDir
   *          basedir for targetfile
   * @param targetRelativePath
   *          relative path from basedir to store target zml file
   */
  public static TimeseriesLinkType copyToTimeseriesLink( final URL copySource, final String axis1Type, final String axis2Type, final File targetBaseDir, final String targetRelativePath, final boolean relative, final boolean simulateCopy ) throws Exception
  {
    final File targetZmlFile = new File( targetBaseDir, targetRelativePath );
    final File dir = targetZmlFile.getParentFile();
    dir.mkdirs();

    if( !simulateCopy && !DEBUG )
    {
      try
      {
        convert( copySource, axis1Type, axis2Type, targetZmlFile );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
        System.out.println( "could not create ZML, but operation will continue..." ); //$NON-NLS-1$
      }
    }

    if( relative )
      return generateobsLink( targetRelativePath );

    final UrlUtilities urlUtilities = new UrlUtilities();
    final URL targetURL = urlUtilities.resolveURL( targetBaseDir.toURI().toURL(), targetRelativePath );
    return generateobsLink( targetURL.toExternalForm() );
  }

  /**
   * @param location
   *          location of zml data
   */
  public static TimeseriesLinkType generateobsLink( final String location ) throws Exception
  {
    final TimeseriesLinkType link = OF.createTimeseriesLinkType();
    link.setLinktype( "zml" ); //$NON-NLS-1$
    link.setType( "simple" ); //$NON-NLS-1$
    link.setHref( location );
    return link;
  }

  private static void convert( final URL sourceURL, final String axis1Type, final String axis2Type, final File targetZmlFile ) throws Exception
  {
    final PrintWriter writer = new PrintWriter( targetZmlFile );
    generateValueAxisZml( writer, axis1Type, axis2Type, sourceURL );
    writer.close();
  }

  private static void generateValueAxisZml( final PrintWriter buffer, final String axis1Type, final String axis2Type, final URL sourceURL ) throws Exception
  {
    final String location = sourceURL.toExternalForm();

    // TODO: use binding classes instead to create a zml.

    buffer.append( "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>" ); //$NON-NLS-1$
    buffer.append( "    <observation xmlns=\"zml.kalypso.org\" " ); //$NON-NLS-1$
    buffer.append( " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"zml.kalypso.org./observation.xsd\" xmlns:xlink=\"http://www.w3.org/1999/xlink\">" ); //$NON-NLS-1$
    buffer.append( "  <name>-</name>" ); //$NON-NLS-1$
    buffer.append( "      <metadataList>" ); //$NON-NLS-1$
    buffer.append( "      </metadataList>" ); //$NON-NLS-1$

    // axis1
    buffer.append( "<axis name=\"" + TimeserieUtils.getName( axis1Type ) + "\" " //$NON-NLS-1$ //$NON-NLS-2$
        + " type=\"" + axis1Type + "\" unit=\"" + TimeserieUtils.getUnit( axis1Type ) + "\"" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    buffer.append( " >" ); //$NON-NLS-1$
    buffer.append( "<valueLink separator=\",\" column=\"1\" line=\"4\" " ); //$NON-NLS-1$
    buffer.append( " xlink:href=\"" + location + "\"/>" ); //$NON-NLS-1$ //$NON-NLS-2$
    buffer.append( "</axis>" ); //$NON-NLS-1$
    // axis2
    buffer.append( "<axis name=\"" + TimeserieUtils.getName( axis2Type ) + "\" " //$NON-NLS-1$ //$NON-NLS-2$
        + " type=\"" + axis2Type + "\" unit=\"" + TimeserieUtils.getUnit( axis2Type ) + "\"" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    buffer.append( "<valueLink separator=\",\" column=\"2\" line=\"4\" " ); //$NON-NLS-1$
    buffer.append( " xlink:href=\"" + location + "\"/>" ); //$NON-NLS-1$ //$NON-NLS-2$
    buffer.append( "</axis>" ); //$NON-NLS-1$
    buffer.append( "</observation>" ); //$NON-NLS-1$
  }

  public static void createExt2File( final StringBuffer writer, final IObservation observation, final Date start, final Date end, final String axisType, final String defaultValue ) throws IOException, SensorException
  {
    final Ext2Writer extWriter = new Ext2Writer( start, end );
    extWriter.write( observation, axisType, writer, defaultValue );
  }

  public static void createFile( final StringBuffer writer, final String axisValueType, final IObservation observation ) throws Exception
  {
    final IAxis[] axis = observation.getAxisList();
    final IAxis dateAxis = ObservationUtilities.findAxisByType( axis, ITimeseriesConstants.TYPE_DATE );
    final IAxis valueAxis = ObservationUtilities.findAxisByType( axis, axisValueType );

    final ITupleModel values = observation.getValues( null );
    for( int i = 0; i < values.getCount(); i++ )
    {
      final Date date = (Date) values.getElement( i, dateAxis );
      final double value = (Double) values.getElement( i, valueAxis );

      if( i == 0 )
        writeGrapHeader( writer, date );

      writeGrapDate( writer, date, value );
    }
  }

  public static void createSyntheticFile( final StringBuffer writer, final String valueAxisType, final IObservation observation, final Date simulationStart, final Date simulationEnd, final int minutesOfTimeStep ) throws Exception
  {
    if( simulationStart.after( simulationEnd ) )
      return;

    final IAxis[] axis = observation.getAxisList();
    final IAxis valueAxis = ObservationUtilities.findAxisByType( axis, valueAxisType );

    final ITupleModel values = observation.getValues( null );
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

    writeGrapHeader( writer, simulationStart );

    Date currentDate = simulationStart;
    do
    {
      final double value = interpolator.getValue( currentDate );
      writeGrapDate( writer, currentDate, value );
      // FIXME: no! do not handle dates like this, use Calendar!
      currentDate = new Date( currentDate.getTime() + syntheticTimeseriesStep );
    }
    while( currentDate.before( simulationEnd ) );
  }

  // REMARK: we are not using PrintWriter#format for maximum performance
  private static void writeGrapHeader( final StringBuffer writer, final Date date )
  {
    final String dateString = DF_GRAP_HEADER.format( date );

    writer.append( "\n       " );
    writer.append( dateString );
    writer.append( "\ngrap\n" ); //$NON-NLS-1$
  }

  // REMARK: sometimes values < 0 are used to indicate measure-failures,
  // unfortunately the simulation kernel will hang in an endless loop and write a endless
  // error file if this occurs, so we better prevent this in any case
  // FIXME: an exception should be thrown in that case. Kalypso should prevent negative numbers to be imported into
  // its timeseries.
  // REMARK: we are not using PrintWriter#format for maximum performance
  private static void writeGrapDate( final StringBuffer writer, final Date date, final double value )
  {
    final String grapDate = GRAP_DATE_FORMAT.format( date );

    final double valueToWrite = value < 0.0 ? 0.0 : value;

    writer.append( grapDate ).append( ' ' );

    DoubleToString.appendFormattedNoThousands( writer, valueToWrite, 3, '.' );

// GRAP_VALUE_FORMAT.format( valueToWrite, writer, NO_OP_FIELD_POSITION );

    writer.append( '\n' );
  }
}