/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.model.hydrology.internal.preprocessing.writer;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Calendar;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.model.hydrology.internal.NATimeSettings;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.request.IRequest;
import org.kalypso.ogc.sensor.request.ObservationRequest;
import org.kalypso.ogc.sensor.timeseries.interpolation.InterpolationFilter;

/**
 * Writes an observation as EXT-File, but writes it as daily values, from the last year until +1 year.
 * 
 * @author Gernot Belger
 */
class Ext2InterpolationWriter
{
  private final IObservation m_observation;

  private final String m_valueAxisType;

  private final String m_defaultValue;

  public Ext2InterpolationWriter( final IObservation observation, final String valueAxisType, final String defaultValue )
  {
    m_observation = observation;
    m_valueAxisType = valueAxisType;
    m_defaultValue = defaultValue;
  }

  public void write( final File targetFile, final DateRange simulationRange ) throws SensorException
  {
    final DateRange extendedRange = createExtendedRange( simulationRange );
    final IRequest request = new ObservationRequest( extendedRange );

    final InterpolationFilter filter = new InterpolationFilter( Calendar.HOUR_OF_DAY, 24, true, m_defaultValue, IStatus.WARNING, false );
    filter.initFilter( null, m_observation, null );

    PrintWriter writer = null;
    try
    {
      writer = new PrintWriter( targetFile );
      final Ext2Writer extWriter = new Ext2Writer();
      extWriter.write( filter, request, m_valueAxisType, writer );
      writer.flush();
      writer.close();
    }
    catch( final IOException e )
    {
      IOUtils.closeQuietly( writer );
    }
  }

  private DateRange createExtendedRange( final DateRange simulationRange )
  {
    final Calendar calendarStart = NATimeSettings.getInstance().getCalendar( simulationRange.getFrom() );
    calendarStart.set( Calendar.DAY_OF_YEAR, 0 );

    if( calendarStart.get( Calendar.HOUR_OF_DAY ) >= 12 )
      calendarStart.add( Calendar.DAY_OF_MONTH, -1 );

    calendarStart.set( Calendar.HOUR_OF_DAY, 12 );
    calendarStart.set( Calendar.MINUTE, 0 );

    final Calendar calendarEnd = NATimeSettings.getInstance().getCalendar( simulationRange.getTo() );
    calendarEnd.set( Calendar.MONTH, 11 );
    calendarEnd.set( Calendar.DAY_OF_MONTH, 31 );
    calendarEnd.set( Calendar.HOUR_OF_DAY, 12 );
    calendarEnd.set( Calendar.MINUTE, 0 );
    calendarEnd.add( Calendar.YEAR, 1 );

    return new DateRange( calendarStart.getTime(), calendarEnd.getTime() );
  }

}
