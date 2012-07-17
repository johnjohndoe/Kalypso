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
package org.kalypso.model.hydrology.binding.timeseries;

import java.util.Date;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.CoreException;
import org.joda.time.Period;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Gernot Belger
 */
public interface ITimeseries extends Feature, IParameterTypeProvider
{
  QName FEATURE_TIMESERIES = new QName( NaModelConstants.NS_TIMESERIES_MANAGEMENT, "Timeseries" ); //$NON-NLS-1$

  QName PROPERTY_QUALITY = new QName( NaModelConstants.NS_TIMESERIES_MANAGEMENT, "quality" ); //$NON-NLS-1$

  QName PROPERTY_PARAMETER_TYPE = new QName( NaModelConstants.NS_TIMESERIES_MANAGEMENT, "parameterType" ); //$NON-NLS-1$

  QName PROPERTY_DATA = new QName( NaModelConstants.NS_TIMESERIES_MANAGEMENT, "data" ); //$NON-NLS-1$

  QName PROPERTY_TIMESTEP_AMOUNT = new QName( NaModelConstants.NS_TIMESERIES_MANAGEMENT, "timestepAmount" ); //$NON-NLS-1$

  QName PROPERTY_TIMESTEP_FIELD = new QName( NaModelConstants.NS_TIMESERIES_MANAGEMENT, "timestepField" ); //$NON-NLS-1$

  QName PROPERTY_MEASUREMENT_START = new QName( NaModelConstants.NS_TIMESERIES_MANAGEMENT, "measurementStart" ); //$NON-NLS-1$

  QName PROPERTY_MEASUREMENT_END = new QName( NaModelConstants.NS_TIMESERIES_MANAGEMENT, "measurementEnd" ); //$NON-NLS-1$

  String getQuality( );

  void setQuality( String quality );

  void setParameterType( String parameterType );

  Period getTimestep( );

  void setTimestep( Period period );

  ZmlLink getDataLink( );

  IStation getStation( );

  Date getMeasurementStart( );

  Date getMeasurementEnd( );

  void setMeasurementStart( Date date );

  void setMeasurementEnd( Date date );

  /**
   * This function returns the measurement date range ({@link #getMeasurementStart()} and {@link #getMeasurementEnd()}).
   * If the measurement range is not correctly set, it will be determined via the observation and will then be set.
   * Afterwards it is returned.
   *
   * @return The measurement date range.
   */
  DateRange getDateRange( );

  void deleteDataFile( ) throws CoreException;
}