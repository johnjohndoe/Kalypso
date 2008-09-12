/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.dwd;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.net.URL;
import java.text.DateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Properties;
import java.util.logging.Level;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import org.apache.commons.io.IOUtils;
import org.kalypso.commons.bind.JaxbUtilities;
import org.kalypso.contribs.java.net.UrlResolver;
import org.kalypso.contribs.java.util.logging.ILogger;
import org.kalypso.contribs.java.util.logging.LoggerUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.dwd.dwdzml.DwdzmlConf;
import org.kalypso.dwd.dwdzml.ObjectFactory;
import org.kalypso.dwd.dwdzml.DwdzmlConf.Target;
import org.kalypso.dwd.dwdzml.DwdzmlConf.Target.Map;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.impl.DefaultAxis;
import org.kalypso.ogc.sensor.impl.SimpleObservation;
import org.kalypso.ogc.sensor.impl.SimpleTuppleModel;
import org.kalypso.ogc.sensor.request.ObservationRequest;
import org.kalypso.ogc.sensor.status.KalypsoStati;
import org.kalypso.ogc.sensor.status.KalypsoStatusUtils;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;
import org.kalypso.ogc.sensor.timeseries.forecast.ForecastFilter;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.ogc.sensor.zml.ZmlURL;
import org.kalypso.zml.Observation;

/**
 * @see org.kalypso.dwd.DWDTask
 * @author doemming
 */
public class DWDTaskDelegate
{
  private static final DateFormat DWD_DATEFORMAT = DateFormat.getDateTimeInstance( DateFormat.MEDIUM, DateFormat.SHORT );

  static
  {
    DWD_DATEFORMAT.setTimeZone( KalypsoCorePlugin.getDefault().getTimeZone() );
  }

  private static final JAXBContext JC = JaxbUtilities.createQuiet( ObjectFactory.class );

  private Properties m_metadata = null;

  public void execute( final ILogger logger, final URL obsRasterURL, final URL dwd2zmlConfUrl, final File targetContext, final Date startSim, final Date startForecast, final Date stopSim, String filter, final Properties metadata ) throws Exception
  {
    m_metadata = metadata;

    logger.log( Level.FINE, LoggerUtilities.CODE_NONE, " inputRaster: " + obsRasterURL );
    logger.log( Level.FINE, LoggerUtilities.CODE_NONE, " raster to zml mapping: " + dwd2zmlConfUrl );
    logger.log( Level.FINE, LoggerUtilities.CODE_NONE, " context for targets (zml-files): " + targetContext );
    logger.log( Level.FINE, LoggerUtilities.CODE_NONE, " unmarshall dwd2zml configuration ..." );

    final Unmarshaller unmarshaller = JC.createUnmarshaller();
    final DwdzmlConf conf = (DwdzmlConf) unmarshaller.unmarshal( dwd2zmlConfUrl );
    final String axisType = DWDRasterHelper.getAxisTypeForDWDKey( conf.getDwdKey() );
    logger.log( Level.FINE, LoggerUtilities.CODE_NONE, " type of ZML to generate" + axisType );

    logger.log( Level.FINE, LoggerUtilities.CODE_NONE, " read inputraster..." );
    DWDObservationRaster obsRaster = null;
    try
    {
      obsRaster = DWDRasterHelper.loadObservationRaster( obsRasterURL, conf.getDwdKey(), conf.getNumberOfCells() );
    }
    catch( final Exception e )
    {
      logger.log( Level.SEVERE, LoggerUtilities.CODE_SHOW_MSGBOX, "Konnte Raster nicht laden, DWD-Vorhersage kann nicht verwendet werden: " + e.getLocalizedMessage() );
    }

    if( obsRaster != null )
    {
      final Calendar baseDateCal = Calendar.getInstance();
      baseDateCal.setTime( obsRaster.getBaseDate() );

      // REMARK: as the DWD Date wa previously read with UTC-1 (probably wrongly), because of the Saale-Model (HWVOR00),
      // we now delete that hour from the date in order to have a nice message with even dates 0/12 hours.
      // This has to be corrected as sson as we know what timeszone the DWD-LM-Data is truly in.
      baseDateCal.add( Calendar.HOUR, -1 );

      final Date firstRasterDate = baseDateCal.getTime();

      /* Produce warning, if rasterDate is much before startForecast */
      if( firstRasterDate == null || startForecast == null )
        logger.log( Level.SEVERE, LoggerUtilities.CODE_SHOW_MSGBOX, "Startdatum der DWD-Rasterdaten konnte nicht ermittelt werden oder Vorhersagezeitpunkt ist nicht gesetzt. Prüfen Sie den Dateneingang." );
      else
      {
        final long distance = startForecast.getTime() - firstRasterDate.getTime();
        final Double distanceInHours = new Double( (double) distance / (1000 * 60 * 60) );

        final String msg = String.format( "DWD-Lokalmodell vom %s", new Object[] { DWD_DATEFORMAT.format( firstRasterDate ) } );

        if( distanceInHours.doubleValue() > 12 )
        {
          logger.log( Level.WARNING, LoggerUtilities.CODE_SHOW_MSGBOX, "DWD Daten veraltet, bitte prüfen Sie den Dateneingang." );
          logger.log( Level.INFO, LoggerUtilities.CODE_SHOW_DETAILS, msg );
        }
        else if( distanceInHours.doubleValue() < -12 )
        {
          logger.log( Level.WARNING, LoggerUtilities.CODE_SHOW_MSGBOX, "Vorhersagezeitraum liegt deutlich (>12h) vor dem Datum der DWD Daten. Bitte prüfen Sie den Dateneingang." );
          logger.log( Level.INFO, LoggerUtilities.CODE_SHOW_DETAILS, msg );
        }
        else
          // in order to always have a message, if nothing else happens make this mesage a main message
          logger.log( Level.INFO, LoggerUtilities.CODE_SHOW_MSGBOX, msg );
      }
    }
    final List<Target> targetList = conf.getTarget();
    if( filter == null )
      filter = "";
    else
      logger.log( Level.FINE, LoggerUtilities.CODE_NONE, "benutze Filter: " + filter );

    logger.log( Level.FINE, LoggerUtilities.CODE_NONE, "Zeitraum " );
    logger.log( Level.FINE, LoggerUtilities.CODE_NONE, " von " + startSim + " (Messung" );
    logger.log( Level.FINE, LoggerUtilities.CODE_NONE, " von " + startForecast + " (Vorhersage)" );
    logger.log( Level.FINE, LoggerUtilities.CODE_NONE, " bis " + stopSim );
    logger.log( Level.FINE, LoggerUtilities.CODE_NONE, " generate zml..." );
    // iterate zml to generate
    
    for( final Target targetZML : targetList )
    {
      final String targetZMLref = targetZML.getTargetZR();
      final File resultFile = new File( targetContext, targetZMLref );

      // iterate hours

      final Date[] dates;
      if( obsRaster != null )
        dates = obsRaster.getDates( startForecast, stopSim );
      else
        dates = new Date[0];
      final Object[][] tupleData = new Object[dates.length][3];
      for( int i = 0; i < dates.length; i++ )
      {
        final Date date = dates[i];
        // iterate rastercells
        final List<Map> map = targetZML.getMap();
        double value = 0;
        
        for( final Map mapping : map )
        {
          final int cellPos = mapping.getCellPos();
          final double factor = mapping.getFactor();
          value += factor * obsRaster.getValueFor( date, cellPos );
        }
        tupleData[i][0] = date;
        tupleData[i][1] = new Double( value );
        tupleData[i][2] = new Integer( KalypsoStati.BIT_OK );
      }
      resultFile.getParentFile().mkdirs();

      try
      {
        final IAxis dateAxis = new DefaultAxis( "Datum", TimeserieConstants.TYPE_DATE, "", Date.class, true, true );
        final String title = TimeserieUtils.getName( axisType );
        final IAxis valueAxis = new DefaultAxis( title, axisType, TimeserieUtils.getUnit( axisType ), TimeserieUtils.getDataClass( axisType ), false, true );
        final IAxis statusAxis = KalypsoStatusUtils.createStatusAxisFor( valueAxis, true );
        final IAxis[] axis = new IAxis[] { dateAxis, valueAxis, statusAxis };

        final ITuppleModel tupleModel = new SimpleTuppleModel( axis, tupleData );

        final MetadataList metadataList = new MetadataList();

        final IObservation dwdObservation = new SimpleObservation( "href", "ID", title, false, null, metadataList, axis, tupleModel );

        final IObservation forecastObservation;
        // generate href from filter and intervall
        final String href = ZmlURL.insertRequest( filter, new ObservationRequest( startForecast, stopSim ) );
        forecastObservation = ZmlFactory.decorateObservation( dwdObservation, href, targetContext.toURL() );

        // ----------------
        // merge with target:
        // load target
        final ObservationRequest observationRequest = new ObservationRequest( startSim, startForecast );
        final String sourceref = ZmlURL.insertRequest( targetZMLref, observationRequest );

        final URL sourceURL = new UrlResolver().resolveURL( targetContext.toURL(), sourceref );
        IObservation targetObservation = null;
        try
        {
          targetObservation = ZmlFactory.parseXML( sourceURL, title );
        }
        catch( Exception e )
        {
          // nothing, if target is not existing it will be ignored
        }
        final ForecastFilter fc = new ForecastFilter();
        final IObservation[] srcObs;
        if( targetObservation != null )
          srcObs = new IObservation[] { targetObservation, forecastObservation };
        else
          srcObs = new IObservation[] { forecastObservation };

        // important: the forecast-filter is based on the target-obs (if existing)
        // in order to keep its metadata & co
        final IObservation baseObservation;
        if( targetObservation != null )
          baseObservation = targetObservation;
        else
          baseObservation = new SimpleObservation( axis );

        fc.initFilter( srcObs, baseObservation, targetContext.toURL() );
        TimeserieUtils.setForecast( fc, startForecast, stopSim );

        // ----------------
        // add all the metadata from task-parameters
        fc.getMetadataList().putAll( m_metadata );
        //
        final Observation observationType = ZmlFactory.createXML( fc, null );
        final Marshaller marshaller = ZmlFactory.getMarshaller();
        marshaller.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE );

        final FileOutputStream fileWriter = new FileOutputStream( resultFile );
        final OutputStreamWriter streamWriter = new OutputStreamWriter( fileWriter, "UTF-8" );
        try
        {
          marshaller.marshal( observationType, streamWriter );
        }
        finally
        {
          IOUtils.closeQuietly( streamWriter );
          IOUtils.closeQuietly( fileWriter );
        }
      }
      catch( final Exception e )
      {
        throw e;
      }
    }
  }
}
