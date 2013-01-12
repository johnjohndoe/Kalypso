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
package org.kalypso.model.hydrology.internal.preprocessing.writer;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.SortedMap;
import java.util.TreeMap;

import org.kalypso.contribs.java.lang.DoubleToString;
import org.kalypso.model.hydrology.internal.NATimeSettings;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITupleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;

/**
 * @author Gernot Belger
 */
class GrapWriter
{
  private static final DateFormat DF_GRAP_HEADER = NATimeSettings.getInstance().getTimeZonedDateFormat( new SimpleDateFormat( "'\n       'yyyyMMddHHmm'\ngrap\n'" ) ); //$NON-NLS-1$

  private static final DateFormat GRAP_DATE_FORMAT = NATimeSettings.getInstance().getTimeZonedDateFormat( new SimpleDateFormat( "dd MM yyyy HH mm ss" ) ); //$NON-NLS-1$

  private static final RangeFactor DEFAULT_RANGE_FACTOR = new RangeFactor( null, 1.0 );

  private final String m_valueAxisType;

  private final IObservation m_observation;

  private RangeFactor m_factor = DEFAULT_RANGE_FACTOR;

  public GrapWriter( final String valueAxisType, final IObservation observation )
  {
    m_valueAxisType = valueAxisType;
    m_observation = observation;
  }

  /**
   * Set a {@link RangeFactor}. Each value is written to the currently set {@link RangeFactor} before it gets written.
   */
  public void setRangeFactor( final RangeFactor factor )
  {
    m_factor = factor;
    /* Set factor that does nothing if set to null */
    if( m_factor == null )
      m_factor = DEFAULT_RANGE_FACTOR;
  }

  public void write( final StringBuilder writer ) throws SensorException
  {
    final IAxis[] axis = m_observation.getAxes();
    final IAxis dateAxis = ObservationUtilities.findAxisByType( axis, ITimeseriesConstants.TYPE_DATE );
    final IAxis valueAxis = ObservationUtilities.findAxisByType( axis, m_valueAxisType );

    final ITupleModel values = m_observation.getValues( null );
    for( int i = 0; i < values.size(); i++ )
    {
      final Date date = (Date)values.get( i, dateAxis );
      final double value = (Double)values.get( i, valueAxis );

      if( i == 0 )
        writeGrapHeader( writer, date );

      writeGrapDate( writer, date, value, m_factor );
    }
  }

  /**
   * Forces m_obseration into the given range by a linear transformation of the date's. This is most probably not was
   * intended.
   */
  @Deprecated
  public void writeSyntheticFile( final StringBuilder writer, final Date simulationStart, final Date simulationEnd, final int minutesOfTimeStep ) throws SensorException
  {
    if( simulationStart.after( simulationEnd ) )
      return;

    /* Fetch value axis */
    final IAxis[] axis = m_observation.getAxes();
    final IAxis valueAxis = ObservationUtilities.findAxisByType( axis, m_valueAxisType );

    /* Hash dates/values into map and shift to 'simulationStart' */

    // REMARK: stretches/squeezes the input timeseries into the simulation range
    // FIXME: this is nonsense! this only makes sense if length and timestep of input and output ranges are the same

    final ITupleModel values = m_observation.getValues( null );
    final SortedMap<Date, Double> valuesMap = new TreeMap<>();

    // TODO: check if this does a correct interpolation, seems dubious
    final long interpolationStep = (simulationEnd.getTime() - simulationStart.getTime()) / (values.size() - 1);

    for( int i = 0; i < values.size(); i++ )
    {
      final Date date = new Date( simulationStart.getTime() + i * interpolationStep );
      final Double value = (Double)values.get( i, valueAxis );
      valuesMap.put( date, value );
    }

    /* Interpolate onto the raster of the output timeseries */
    final Interpolator interpolator = new Interpolator( valuesMap );
    final long syntheticTimeseriesStep = minutesOfTimeStep * 60000; // milliseconds

    GrapWriter.writeGrapHeader( writer, simulationStart );

    Date currentDate = simulationStart;
    do
    {
      final double value = interpolator.getValue( currentDate );
      GrapWriter.writeGrapDate( writer, currentDate, value );
      // FIXME: no! do not handle dates like this, use Calendar!
      currentDate = new Date( currentDate.getTime() + syntheticTimeseriesStep );
    }
    while( currentDate.before( simulationEnd ) );
  }

  private static void writeGrapHeader( final StringBuilder writer, final Date date )
  {
    // REMARK: we are not using PrintWriter#format for maximum performance
    // TODO: use joda DateTime for even better performance
    final String dateString = DF_GRAP_HEADER.format( date );
    writer.append( dateString );
  }

  // REMARK: sometimes values < 0 are used to indicate measure-failures,
  // unfortunately the simulation kernel will hang in an endless loop and write a endless
  // error file if this occurs, so we better prevent this in any case
  // FIXME: an exception should be thrown in that case. Kalypso should prevent negative numbers to be imported into
  // its timeseries.
  // REMARK: we are not using PrintWriter#format for maximum performance
  private static void writeGrapDate( final StringBuilder writer, final Date date, final double value )
  {
    writeGrapDate( writer, date, value, DEFAULT_RANGE_FACTOR );
  }

  private static void writeGrapDate( final StringBuilder writer, final Date date, final double value, final RangeFactor factor )
  {
    final String grapDate = GRAP_DATE_FORMAT.format( date );

    final double checkedValue = value < 0.0 ? 0.0 : value;
    final double valueToWrite = factor.apply( date, checkedValue );

    writer.append( grapDate ).append( ' ' );

    DoubleToString.appendFormattedNoThousands( writer, valueToWrite, 3, '.' );

    writer.append( '\n' );
  }
}