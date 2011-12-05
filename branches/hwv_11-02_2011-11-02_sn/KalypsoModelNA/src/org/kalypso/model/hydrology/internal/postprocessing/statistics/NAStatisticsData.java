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
package org.kalypso.model.hydrology.internal.postprocessing.statistics;

import java.io.File;
import java.util.Date;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITupleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;

/**
 * @author Gernot Belger
 */
public class NAStatisticsData
{
  private final IObservation m_observation;

  private double m_timestepSeconds;

  private double m_maxValue = -Double.MAX_VALUE;

  private Date m_maxValueDate = null;

  private double m_volume = 0.0;

  private final File m_resultFile;

  public NAStatisticsData( final IObservation observation, final File resultFile )
  {
    m_observation = observation;
    m_resultFile = resultFile;
  }

  public File getResultFile( )
  {
    return m_resultFile;
  }

  public void calculateStatistics( ) throws SensorException
  {
    final IAxis[] axisList = m_observation.getAxes();
    final IAxis dateAxis = ObservationUtilities.findAxisByType( axisList, ITimeseriesConstants.TYPE_DATE );
    final IAxis valueAxis = ObservationUtilities.findAxisByType( axisList, ITimeseriesConstants.TYPE_RUNOFF );
    if( dateAxis == null || valueAxis == null )
      return;

    final ITupleModel tuppleModel = m_observation.getValues( null );

    m_timestepSeconds = findTimestep( dateAxis, tuppleModel );

    for( int i = 0; i < tuppleModel.size(); i++ )
    {
      final Date date = (Date) tuppleModel.get( i, dateAxis );
      final double value = (Double) tuppleModel.get( i, valueAxis );
      addVolume( value );
      calcMaxValue( date, value );
    }
  }

  private void calcMaxValue( final Date date, final double value )
  {
    if( m_maxValue < value )
      m_maxValueDate = date;

    m_maxValue = Math.max( m_maxValue, value );
  }

  private void addVolume( final double value )
  {
    // FIXME: This is actually nonsense and not correct. We need to use the trapzoid formula to determine the
    // volume!
    m_volume += value * m_timestepSeconds;
  }

  private static double findTimestep( final IAxis dateAxis, final ITupleModel tuppleModel ) throws SensorException
  {
    // here we assume constant timestep in the whole timeseries data
    if( tuppleModel.size() < 2 )
      return 0.0;

    final Date date0 = (Date) tuppleModel.get( 0, dateAxis );
    final Date date1 = (Date) tuppleModel.get( 1, dateAxis );
    if( date0 != null && date1 != null )
    {
      final double millisDifference = Math.abs( date1.getTime() - date0.getTime() );
      return millisDifference / 1000.0;
    }
    return 0.0;
  }

  public Date getMaxValueDate( )
  {
    return m_maxValueDate;
  }

  public Double getMaxValue( )
  {
    return m_maxValue;
  }

  public Double getVolume( )
  {
    return m_volume;
  }

}
