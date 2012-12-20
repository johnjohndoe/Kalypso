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

import java.util.Date;
import java.util.List;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.IProgressMonitor;
import org.kalypso.commons.tokenreplace.IStringResolver;
import org.kalypso.model.rcm.RcmConstants;
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
  /**
   * The qname of the filter member.
   */
  QName MEMBER_FILTER = new QName( UrlCatalogRcm.NS_RCM, "filterMember" ); //$NON-NLS-1$

  /**
   * The qname of the period.
   */
  QName PROPERTY_PERIOD = new QName( UrlCatalogRcm.NS_RCM, "period" ); //$NON-NLS-1$

  /**
   * The qname of the model.
   */
  QName PROPERTY_MODEL = new QName( UrlCatalogRcm.NS_RCM, "model" ); //$NON-NLS-1$

  /**
   * The qname of the valid from time.
   */
  QName PROPERTY_VALID_FROM = new QName( RcmConstants.NS_RCM, "validFrom" ); // $NON-NLS-1$ //$NON-NLS-1$

  /**
   * The qname of the valid to time.
   */
  QName PROPERTY_VALID_TO = new QName( RcmConstants.NS_RCM, "validTo" ); // $NON-NLS-1$ //$NON-NLS-1$

  /**
   * The qname of the parameter type.
   */
  QName PROPERTY_PARAMETER_TYPE = new QName( UrlCatalogRcm.NS_RCM, "parameterType" ); //$NON-NLS-1$

  /**
   * The qname of the last modified timestamp.
   */
  QName PROPERTY_LAST_MODIFIED = new QName( UrlCatalogRcm.NS_RCM, "lastModified" ); //$NON-NLS-1$

  /**
   * This function calculates the rainfall for the given catchments.
   * 
   * @param catchmentFeatures
   *          The features of the catchments.
   * @param variables
   *          If this variables are set, the period will be resolved using this variables. The generator does not know,
   *          if his stored period needs to be resolved or not.
   * @param log
   *          The log.
   * @param monitor
   *          A progress monitor.
   */
  IObservation[] createRainfall( final Feature[] catchmentFeatures, IStringResolver variables, ILog log, final IProgressMonitor monitor ) throws CoreException;

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
   * This function returns the valid from time.
   * 
   * @return The valid from time.
   */
  public Date getValidFrom( );

  /**
   * This function sets the valid from time.
   * 
   * @param validFrom
   *          The valid from time.
   */
  public void setValidFrom( final Date validFrom );

  /**
   * This function returns the valid to time.
   * 
   * @return The valid to time.
   */
  public Date getValidTo( );

  /**
   * This function sets the valid to time.
   * 
   * @param validTo
   *          The valid to time.
   */
  public void setValidTo( final Date validTo );

  /**
   * This function returns the parameter type.
   * 
   * @return The parameter type.
   */
  String getParameterType( );

  /**
   * This function sets the parameter type.
   * 
   * @param parameterType
   *          The parameter type.
   */
  void setParameterType( String parameterType );

  /**
   * This function returns the last modified timestamp.
   * 
   * @return The last modified timestamp.
   */
  Date getLastModified( );

  /**
   * This function sets the last modified timestamp.
   * 
   * @param lastModified
   *          The last modified timestamp.
   */
  void setLastModified( Date lastModified );

  /**
   * This function returns the filters.
   * 
   * @return The filters.
   */
  List<IZmlFilter> getFilters( );

  /**
   * This function adds a filter, which will be applied to the source timeseries.
   * 
   * @param filter
   *          A filter, which will be applied to the source timeseries.
   */
  void addFilter( IZmlFilter filter );

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