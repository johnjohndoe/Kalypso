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
package org.kalypso.model.rcm.internal.binding;

import org.joda.time.Period;
import org.kalypso.commons.time.PeriodUtils;
import org.kalypso.contribs.java.util.CalendarUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.rcm.binding.IFactorizedTimeseries;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;

/**
 * The factorized timeseries.
 * 
 * @author Holger Albert
 */
public class FactorizedTimeseries extends Feature_Impl implements IFactorizedTimeseries
{
  /**
   * The constructor.
   * 
   * @param parent
   * @param parentRelation
   * @param ft
   * @param id
   * @param propValues
   */
  public FactorizedTimeseries( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  /**
   * @see org.kalypso.model.rcm.binding.IFactorizedTimeseries#getFactor()
   */
  @Override
  public Double getFactor( )
  {
    return getProperty( PROPERTY_FACTOR, Double.class );
  }

  /**
   * @see org.kalypso.model.rcm.binding.IFactorizedTimeseries#getTimeseriesLink()
   */
  @Override
  public ZmlLink getTimeseriesLink( )
  {
    return new ZmlLink( this, PROPERTY_TIMESERIES_LINK, getWorkspace().getContext() );
  }

  /**
   * @see org.kalypso.model.rcm.binding.IFactorizedTimeseries#getQuality()
   */
  @Override
  public String getQuality( )
  {
    return getProperty( PROPERTY_QUALITY, String.class );
  }

  /**
   * @see org.kalypso.model.rcm.binding.IFactorizedTimeseries#getTimestepAmount()
   */
  @Override
  public Integer getTimestepAmount( )
  {
    return getProperty( PROPERTY_TIMESTEP_AMOUNT, Integer.class );
  }

  /**
   * @see org.kalypso.model.rcm.binding.IFactorizedTimeseries#getTimestepField()
   */
  @Override
  public String getTimestepField( )
  {
    return getProperty( PROPERTY_TIMESTEP_FIELD, String.class );
  }

  /**
   * @see org.kalypso.model.rcm.binding.IFactorizedTimeseries#getTimestep()
   */
  @Override
  public Period getTimestep( )
  {
    final Integer amount = getTimestepAmount();
    final String fieldName = getTimestepField();
    final int field = CalendarUtilities.getCalendarField( fieldName );
    return PeriodUtils.getPeriod( field, amount );
  }
}