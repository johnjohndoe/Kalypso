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
package org.kalypso.model.hydrology.operation.evaporation.test;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Calendar;
import java.util.Date;

import org.eclipse.core.runtime.NullProgressMonitor;
import org.junit.Test;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.model.hydrology.operation.evaporation.ICalculateEvaporationData;
import org.kalypso.model.hydrology.operation.evaporation.WaterbasedEvaporationCalculator;
import org.kalypso.model.hydrology.operation.evaporation.WendlingLandbasedEvaporationCalculator;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.timeseries.AxisUtils;
import org.kalypso.ogc.sensor.timeseries.base.CacheTimeSeriesVisitor;
import org.kalypso.ogc.sensor.timeseries.base.ITimeseriesCache;
import org.kalypso.ogc.sensor.util.DateRanges;
import org.kalypso.ogc.sensor.visitor.IObservationValueContainer;
import org.kalypso.ogc.sensor.visitor.IObservationVisitor;
import org.kalypso.ogc.sensor.zml.ZmlFactory;

import au.com.bytecode.opencsv.CSVWriter;

/**
 * @author Dirk Kuch
 */
public class EvapoationCalculatorTestCase
{
  @Test
  public void testWaterbasedEvaporationCalculator( ) throws SensorException, IOException
  {
    final ITimeseriesCache humidity = CacheTimeSeriesVisitor.cache( ZmlFactory.parseXML( getClass().getResource( "/etc/test/resources/evaporation/water_mean_humidity.zml" ) ) ); //$NON-NLS-1$
    final ITimeseriesCache sunshine = CacheTimeSeriesVisitor.cache( ZmlFactory.parseXML( getClass().getResource( "/etc/test/resources/evaporation/water_sunshine_hours.zml" ) ) ); //$NON-NLS-1$
    final ITimeseriesCache temperature = CacheTimeSeriesVisitor.cache( ZmlFactory.parseXML( getClass().getResource( "/etc/test/resources/evaporation/water_mean_temperature.zml" ) ) ); //$NON-NLS-1$
    final ITimeseriesCache windVelocity = CacheTimeSeriesVisitor.cache( ZmlFactory.parseXML( getClass().getResource( "/etc/test/resources/evaporation/water_mean_wind_velocity.zml" ) ) ); //$NON-NLS-1$

    final DateRange daterange = getDateRange( humidity, sunshine, temperature, windVelocity );

    final ICalculateEvaporationData data = new DummyCalculateEvaporationData( humidity, sunshine, temperature, windVelocity, daterange );

    final WaterbasedEvaporationCalculator calculator = new WaterbasedEvaporationCalculator();
    calculator.init( data );
    calculator.execute( new NullProgressMonitor() );

    final IObservation observation = calculator.getObservation();
    ZmlFactory.writeToFile( observation, new File( FileUtilities.TMP_DIR, "waterbased_evaporation.zml" ) ); //$NON-NLS-1$

    storeCSV( observation, new File( FileUtilities.TMP_DIR, "waterbased_evaporation.csv" ) ); //$NON-NLS-1$
  }

  @Test
  public void testLandbasedEvaporationCalculator( ) throws SensorException, IOException
  {
    final ITimeseriesCache humidity = CacheTimeSeriesVisitor.cache( ZmlFactory.parseXML( getClass().getResource( "/etc/test/resources/evaporation/land_mean_humidity.zml" ) ) ); //$NON-NLS-1$
    final ITimeseriesCache sunshine = CacheTimeSeriesVisitor.cache( ZmlFactory.parseXML( getClass().getResource( "/etc/test/resources/evaporation/land_sunshine_hours.zml" ) ) ); //$NON-NLS-1$
    final ITimeseriesCache temperature = CacheTimeSeriesVisitor.cache( ZmlFactory.parseXML( getClass().getResource( "/etc/test/resources/evaporation/land_mean_temperature.zml" ) ) ); //$NON-NLS-1$
    final ITimeseriesCache windVelocity = CacheTimeSeriesVisitor.cache( ZmlFactory.parseXML( getClass().getResource( "/etc/test/resources/evaporation/land_mean_wind_velocity.zml" ) ) ); //$NON-NLS-1$

    final DateRange daterange = getDateRange( humidity, sunshine, temperature, windVelocity );

    final ICalculateEvaporationData data = new DummyCalculateEvaporationData( humidity, sunshine, temperature, windVelocity, daterange );

    final WendlingLandbasedEvaporationCalculator calculator = new WendlingLandbasedEvaporationCalculator();
    calculator.init( data );
    calculator.execute( new NullProgressMonitor() );

    final IObservation observation = calculator.getObservation();
    ZmlFactory.writeToFile( observation, new File( FileUtilities.TMP_DIR, "landbased_evaporation.zml" ) ); //$NON-NLS-1$

    storeCSV( observation, new File( FileUtilities.TMP_DIR, "landbased_evaporation.csv" ) ); //$NON-NLS-1$
  }

  private void storeCSV( final IObservation observation, final File target ) throws SensorException, IOException
  {
    final CSVWriter writer = new CSVWriter( new FileWriter( target ), '\t' ); //$NON-NLS-1$

    final IAxis dateAxis = AxisUtils.findDateAxis( observation.getAxes() );
    final IAxis valueAxis = AxisUtils.findValueAxis( observation.getAxes() );

    observation.accept( new IObservationVisitor()
    {
      @Override
      public void visit( final IObservationValueContainer container ) throws SensorException
      {
        final Date date = (Date) container.get( dateAxis );
        final Number value = (Number) container.get( valueAxis );

        final Calendar cal = Calendar.getInstance();
        cal.setTime( date );

        final String day = String.format( "%d", cal.get( Calendar.DAY_OF_MONTH ) ); //$NON-NLS-1$
        final String month = String.format( "%d", cal.get( Calendar.MONTH ) + 1 ); //$NON-NLS-1$
        final String year = String.format( "%d", cal.get( Calendar.YEAR ) ); //$NON-NLS-1$
        final String valueString = String.format( "%.16f", value ); //$NON-NLS-1$

        writer.writeNext( new String[] { day, month, year, valueString } );
      }
    }, null, 1 );

    writer.close();
  }

  private DateRange getDateRange( final ITimeseriesCache... caches ) throws SensorException
  {
    DateRange intersection = null;
    for( final ITimeseriesCache cache : caches )
    {
      intersection = DateRanges.intersect( cache.getDateRange(), intersection );
    }

    return intersection;
  }
}