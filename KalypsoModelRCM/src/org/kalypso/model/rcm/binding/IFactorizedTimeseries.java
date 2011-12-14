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

import javax.xml.namespace.QName;

import org.joda.time.Period;
import org.kalypso.model.rcm.RcmConstants;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypsodeegree.model.feature.Feature;

/**
 * The factorized timeseries.
 * 
 * @author Holger Albert
 */
public interface IFactorizedTimeseries extends Feature
{
  /**
   * The qname of the factorized timeseries.
   */
  public static QName FEATURE_FACTORIZED_TIMESERIES = new QName( RcmConstants.NS_CM, "FactorizedTimeseries" ); //$NON-NLS-1$

  /**
   * The qname of the factor.
   */
  public static QName PROPERTY_FACTOR = new QName( RcmConstants.NS_CM, "factor" ); //$NON-NLS-1$

  /**
   * The qname of the timeseries link.
   */
  public static QName PROPERTY_TIMESERIES_LINK = new QName( RcmConstants.NS_CM, "timeseriesLink" ); //$NON-NLS-1$

  /**
   * The qname of the quality.
   */
  public static QName PROPERTY_QUALITY = new QName( RcmConstants.NS_CM, "quality" ); //$NON-NLS-1$

  /**
   * The qname of the timestep amount.
   */
  public static QName PROPERTY_TIMESTEP_AMOUNT = new QName( RcmConstants.NS_CM, "timestepAmount" ); //$NON-NLS-1$

  /**
   * The qname of the timestep field.
   */
  public static QName PROPERTY_TIMESTEP_FIELD = new QName( RcmConstants.NS_CM, "timestepField" ); //$NON-NLS-1$

  /**
   * This function returns the factor.
   * 
   * @return The factor.
   */
  public Double getFactor( );

  /**
   * This function returns the timeseries link.
   * 
   * @return The timeseries link.
   */
  public ZmlLink getTimeseriesLink( );

  /**
   * This function returns the quality.
   * 
   * @return The quality.
   */
  public String getQuality( );

  /**
   * This function returns the timestep amount.
   * 
   * @return The timestep amount.
   */
  public Integer getTimestepAmount( );

  /**
   * This function returns the timestep field.
   * 
   * @return The timestep field.
   */
  public String getTimestepField( );

  /**
   * This function returns the timestep.
   * 
   * @return The timestep.
   */
  public Period getTimestep( );
}