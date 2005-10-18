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
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;

import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import org.apache.commons.io.IOUtils;
import org.kalypso.commons.java.net.UrlResolver;
import org.kalypso.contribs.java.util.logging.ILogger;
import org.kalypso.dwd.dwdzml.DwdzmlConf;
import org.kalypso.dwd.dwdzml.ObjectFactory;
import org.kalypso.dwd.dwdzml.DwdzmlConfType.TargetType;
import org.kalypso.dwd.dwdzml.DwdzmlConfType.TargetType.MapType;
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
import org.kalypso.zml.ObservationType;

/**
 * @see org.kalypso.dwd.DWDTask
 * 
 * @author doemming
 */
public class DWDTaskDelegate
{
  private Properties m_metadata = null;

  public void execute( final ILogger logger, final URL obsRasterURL, final URL dwd2zmlConfUrl,
      final File targetContext, final Date startSim, final Date startForecast, final Date stopSim, String filter,
      final Properties metadata ) throws Exception
  {
    m_metadata = metadata;
    logger.log( "DWD-task: generates ZML files from DWD-forecast" );
    logger.log( " inputRaster: " + obsRasterURL );
    logger.log( " raster to zml mapping: " + dwd2zmlConfUrl );
    logger.log( " context for targets (zml-files): " + targetContext );
    logger.log( " unmarshall dwd2zml configuration ..." );

    final ObjectFactory dwd2ZmlFac = new ObjectFactory();
    final Unmarshaller unmarshaller = dwd2ZmlFac.createUnmarshaller();
    final DwdzmlConf conf = (DwdzmlConf)unmarshaller.unmarshal( dwd2zmlConfUrl );
    final String axisType = DWDRasterHelper.getAxisTypeForDWDKey( conf.getDwdKey() );
    logger.log( " type of ZML to generate" + axisType );

    logger.log( " read inputraster..." );
    DWDObservationRaster obsRaster = null;
    try
    {
      obsRaster = DWDRasterHelper.loadObservationRaster( obsRasterURL, conf.getDwdKey(), conf.getNumberOfCells() );
    }
    catch( Exception e )
    {
      logger.log( "konnte Raster nicht laden, DWD-Vorhersage kann nicht verwendet werden" );
      logger.log( e.getLocalizedMessage() );
    }
    final List targetList = conf.getTarget();
    if( filter == null )
      filter = "";
    else
      logger.log( "benutze Filter: " + filter );
    logger.log( "Zeitraum " );
    logger.log( " von " + startSim + " (Messung" );
    logger.log( " von " + startForecast + " (Vorhersage)" );
    logger.log( " bis " + stopSim );
    logger.log( " generate zml..." );
    // iterate zml to generate
    for( Iterator iter = targetList.iterator(); iter.hasNext(); )
    {
      final TargetType targetZML = (TargetType)iter.next();
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
        final List map = targetZML.getMap();
        double value = 0;
        for( Iterator iterator = map.iterator(); iterator.hasNext(); )
        {
          final MapType mapping = (MapType)iterator.next();
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
        final IAxis valueAxis = new DefaultAxis( title, axisType, TimeserieUtils.getUnit( axisType ), TimeserieUtils
            .getDataClass( axisType ), false, true );
        final IAxis statusAxis = KalypsoStatusUtils.createStatusAxisFor( valueAxis, true );
        final IAxis[] axis = new IAxis[]
        {
            dateAxis,
            valueAxis,
            statusAxis };

        final ITuppleModel tupleModel = new SimpleTuppleModel( axis, tupleData );

        final MetadataList metadataList = new MetadataList();

        final IObservation dwdObservation = new SimpleObservation( "href", "ID", title, false, null, metadataList,
            axis, tupleModel );
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
          srcObs = new IObservation[]
          {
              targetObservation,
              forecastObservation };
        else
          srcObs = new IObservation[]
          { forecastObservation };

        // important: the forecast-filter is based on the target-obs (if existing)
        // in order to keep its metadata & co
        final IObservation baseObservation;
        if( targetObservation != null )
          baseObservation = targetObservation;
        else
          baseObservation = new SimpleObservation( axis );

        fc.initFilter( srcObs, baseObservation, targetContext.toURL() );

        // ----------------
        // add all the metadata from task-parameters
        fc.getMetadataList().putAll( m_metadata );
        //
        final ObservationType observationType = ZmlFactory.createXML( fc, null );
        final Marshaller marshaller = ZmlFactory.getMarshaller();
        marshaller.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE );

        final FileOutputStream fileWriter = new FileOutputStream( resultFile );
        final OutputStreamWriter streamWriter = new OutputStreamWriter( fileWriter, "UTF-8" );
        try
        {
          marshaller.marshal( observationType, streamWriter );
        }
        catch( Exception e )
        {
          // nothing
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
