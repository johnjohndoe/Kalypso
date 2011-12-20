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
package org.kalypso.model.rcm.binding;

import java.util.List;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.IProgressMonitor;
import org.kalypso.commons.tokenreplace.IStringResolver;
import org.kalypso.model.rcm.internal.UrlCatalogRcm;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.zml.core.filter.binding.IZmlFilter;
import org.kalypsodeegree.model.feature.Feature;

/**
 * GML-binding for the rcm:RainfallGenerator's.
 * 
 * @author Gernot Belger
 */
public interface IRainfallGenerator extends Feature
{
  QName MEMBER_FILTER = new QName( UrlCatalogRcm.NS_RCM, "filterMember" );

  QName PROPERTY_PERIOD = new QName( UrlCatalogRcm.NS_RCM, "period" ); //$NON-NLS-1$

  QName PROPERTY_MODEL = new QName( UrlCatalogRcm.NS_RCM, "model" ); //$NON-NLS-1$

  QName PROPERTY_PARAMETER_TYPE = new QName( UrlCatalogRcm.NS_RCM, "parameterType" ); //$NON-NLS-1$

  /**
   * @param sourceFilter
   *          Enforces source timeseries to have equal properties (length, interval, ...). May be null.
   */
  IObservation[] createRainfall( final Feature[] catchmentFeatures, final DateRange range, ILog log, final IProgressMonitor monitor ) throws CoreException;

  /**
   * This function returns the period. This will be the unmodified period from the feature, containing strings, which
   * may be variables, which needs to be resolved.
   * 
   * @return The period.
   */
  IDateRange getPeriod( );

  /**
   * This function returns the period. This will be a modified period, containing already resolved dates.
   * 
   * @return The period.
   */
  DateRange getPeriod( IStringResolver variables );

  /**
   * This function sets the period.
   * 
   * @param period
   *          The period.
   */
  void setPeriod( DateRange period );

  /**
   * This function returns the models this generator handles.
   * 
   * @return The models.
   */
  String[] getModels( );

  /**
   * This function returns the parameter type.
   * 
   * @return The parameter type.
   */
  String getParameterType( );

  /**
   * This function adds a filter, which will be applied to the source timeseries.
   * 
   * @param filter
   *          A filter, which will be applied to the source timeseries.
   */
  void addFilter( IZmlFilter filter );

  /**
   * This function returns the filters.
   * 
   * @return The filters.
   */
  List<IZmlFilter> getFilters( );

  /**
   * This function adds a filter, which will be applied to the source timeseries.
   * 
   * @param calendarField
   * @param amount
   * @param forceFill
   * @param defaultValue
   * @param defaultStatus
   */
  void addInterpolationFilter( String calendarField, int amount, boolean forceFill, String defaultValue, int defaultStatus );

  /**
   * This function adds a filter, which will be applied to the source timeseries.
   * 
   * @param calendarField
   * @param amount
   * @param defaultValue
   * @param defaultStatus
   */
  void addIntervalFilter( String calendarField, int amount, double defaultValue, int defaultStatus );
}