/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.lhwsachsenanhalt.saale.batch;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.UnsupportedEncodingException;
import java.text.ParseException;
import java.util.Calendar;
import java.util.Date;
import java.util.logging.Logger;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;

import org.apache.commons.io.IOUtils;
import org.kalypso.commons.factory.FactoryException;
import org.kalypso.lhwsachsenanhalt.saale.HWVOR00Converter;
import org.kalypso.ogc.sensor.IAxisRange;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;
import org.kalypso.ogc.sensor.timeseries.wq.WQException;
import org.kalypso.ogc.sensor.timeseries.wq.wqtable.WQTable;
import org.kalypso.ogc.sensor.timeseries.wq.wqtable.WQTableFactory;
import org.kalypso.ogc.sensor.timeseries.wq.wqtable.WQTableSet;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.zml.ObjectFactory;

public class HWVORBatch
{
  private Logger m_logger = Logger.getLogger( HWVORBatch.class.getName() );
  private Marshaller m_marshaller;

  /**
   * Syntax: HWVORBatch <inputdir><outputdir>[ <wqdir>]
   * 
   * <p>
   * if <wqdir>is given, a wq-table with for each W or Q Timeserie is read and added to the observation
   * </p>
   */
  public void convert( final String[] args ) throws Exception
  {
    if( args.length < 2 )
    {
      System.out.println( "Usage: HWVORBatch <inputdir> <outputdir> [<wqdir>]" );
      return;
    }

    final File outputDir = new File( args[1] );

    final File wqdir;
    if( args.length > 2 )
      wqdir = new File( args[2] );
    else
      wqdir = null;

    final File inputDir = new File( args[0] );
    final File wasserDir = new File( outputDir, "Wasserstand" );
    final File durchDir = new File( outputDir, "Durchfluﬂ" );
    final File speicherDir = new File( outputDir, "Speicherinhalt" );
    final File temperaturDir = new File( outputDir, "Temperatur" );
    //    File wqDir = new File( outputDir, "WQ" );
    final File niederDir = new File( outputDir, "Niederschlag" );
    final File schneeDir = new File( outputDir, "Schnee" );

    final String files[] = inputDir.list();

    for( int i = 0; i < files.length; i++ )
    {
      String type;
      File outDir;

      final String file = files[i].toUpperCase();
      if( file.endsWith( ".VOR" ) )
      {
        if( file.startsWith( "W_" ) )
        {
          outDir = wasserDir;
          type = TimeserieConstants.TYPE_WATERLEVEL;
        }
        else if( file.startsWith( "Q_" ) )
        {
          outDir = durchDir;
          type = TimeserieConstants.TYPE_RUNOFF;
        }
        else if( file.startsWith( "TS" ) )
        {
          outDir = speicherDir;
          type = TimeserieConstants.TYPE_VOLUME;
        }
        //        else if( files[i].startsWith( "WQ" ) )
        //        {
        //          currDir = wqDir;
        //          sValue = TimeserieConstants.MD_WQ;
        //        }
        else if( file.startsWith( "P_" ) )
        {
          outDir = niederDir;
          type = TimeserieConstants.TYPE_RAINFALL;
        }
        else if( file.startsWith( "SN" ) )
        {
          outDir = schneeDir;
          type = TimeserieConstants.TYPE_RAINFALL;
        }
        else if( file.startsWith( "TL" ) )
        {
          outDir = temperaturDir;
          type = TimeserieConstants.TYPE_TEMPERATURE;
        }
        else if( file.startsWith( "HWABLAUF" ) )
          continue;
        else
        {
          outDir = outputDir;
          type = TimeserieConstants.TYPE_TEMPERATURE;
        }

        final File vorFile = new File( outDir, file );

        convertFileToZml( vorFile, wqdir, outDir, type );
      }
    }
  }

  public void convertFileToZml( final File vorFile, final File wqdir, final File outDir, final String type )
      throws ParseException, FileNotFoundException, WQException, SensorException, FactoryException,
      UnsupportedEncodingException, JAXBException, IOException
  {
    final MetadataList metadata = new MetadataList();
    metadata.put( "FILE_NAME", vorFile.getAbsolutePath() );

    final IObservation[] obs = readObservations( vorFile, type, metadata );

    outDir.mkdirs();
    for( int l = 0; l < obs.length; l++ )
    {
      final IObservation observation = obs[l];
      final String name = observation.getName();
      final MetadataList md = observation.getMetadataList();

      // W/Q Table for W and Q Timeseries
      if( wqdir != null && ( type == TimeserieConstants.TYPE_RUNOFF || type == TimeserieConstants.TYPE_WATERLEVEL ) )
      {
        final String wqName = "DT" + name;
        final File wqFile = new File( wqdir, wqName );
        final String wqBasename = wqFile.getAbsolutePath();
        final WQTable table = Dbf2WQ.readWQ( wqBasename, new Date( 0 ) );
        if( table != null )
        {
          final WQTableSet wqset = new WQTableSet( new WQTable[]
          { table }, TimeserieConstants.TYPE_WATERLEVEL, TimeserieConstants.TYPE_RUNOFF );
          md.setProperty( TimeserieConstants.MD_WQTABLE, WQTableFactory.createXMLString( wqset ) );
        }
      }

      setForecastHours( observation );

      final File outFile = new File( outDir, "ID" + name + ".zml" );

      final FileOutputStream stream = new FileOutputStream( outFile );
      final OutputStreamWriter writer = new OutputStreamWriter( stream, "UTF-8" );

      final Marshaller marshaller = getMarshaller();
      marshaller.marshal( ZmlFactory.createXML( observation, null ), writer );
      writer.close();
    }
  }

  /**
   * Lazy get marshaller
   * 
   * @throws JAXBException
   */
  private Marshaller getMarshaller() throws JAXBException
  {
    if( m_marshaller == null )
    {
      final ObjectFactory zmlFac = new ObjectFactory();

      m_marshaller = zmlFac.createMarshaller();
      m_marshaller.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE );
    }
    
    return m_marshaller;
  }

  private IObservation[] readObservations( final File vorFile, String sValue, final MetadataList metadata )
      throws FileNotFoundException, ParseException, IOException
  {
    m_logger.info( "Lese Zeitreihen: " + vorFile );
    Reader fileReader = null;
    try
    {
      fileReader = new FileReader( vorFile );
      final IObservation[] obs = HWVOR00Converter.toZML( sValue, fileReader, metadata );
      fileReader.close();
      return obs;
    }
    finally
    {
      IOUtils.closeQuietly( fileReader );
    }
  }

  /**
   * Adds forecast hours to metadata of the given obs: first-date + 120h until last-date
   */
  private void setForecastHours( final IObservation observation ) throws SensorException
  {
    final ITuppleModel values = observation.getValues( null );
    final IAxisRange dateRange = values.getRangeFor( ObservationUtilities.findAxisByType( observation.getAxisList(),
        TimeserieConstants.TYPE_DATE ) );
    final Calendar startCal = Calendar.getInstance();
    startCal.setTime( (Date)dateRange.getLower() );
    startCal.add( Calendar.HOUR_OF_DAY, 119 ); // die ersten 120 Stunden sind Messwerte
    TimeserieUtils.setForecast( observation, startCal.getTime(), (Date)dateRange.getUpper() );
  }

  public static void main( String[] args ) throws Exception
  {
    new HWVORBatch().convert( args );
  }
}
