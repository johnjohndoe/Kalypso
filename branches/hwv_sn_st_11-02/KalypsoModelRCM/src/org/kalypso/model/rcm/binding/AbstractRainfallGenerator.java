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
package org.kalypso.model.rcm.binding;

import javax.xml.namespace.QName;

import org.apache.commons.lang.StringUtils;
import org.kalypso.commons.tokenreplace.IStringResolver;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.rcm.internal.UrlCatalogRcm;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;

/**
 * @author Gernot Belger
 */
public abstract class AbstractRainfallGenerator extends Feature_Impl implements IRainfallGenerator
{
  private static final QName PROPERTY_PERIOD = new QName( UrlCatalogRcm.NS_RCM, "period" ); //$NON-NLS-1$

  private static final QName PROPERTY_MODEL = new QName( UrlCatalogRcm.NS_RCM, "model" ); //$NON-NLS-1$

  protected AbstractRainfallGenerator( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  /**
   * @see org.kalypso.model.rcm.binding.IRainfallGenerator#getPeriod()
   */
  @Override
  public org.kalypso.model.rcm.internal.binding.DateRange getPeriod( )
  {
    return getProperty( PROPERTY_PERIOD, org.kalypso.model.rcm.internal.binding.DateRange.class );
  }

  /**
   * @see org.kalypso.model.rcm.binding.IRainfallGenerator#getPeriod(org.kalypso.commons.tokenreplace.IStringResolver)
   */
  @Override
  public DateRange getPeriod( final IStringResolver variables )
  {
    return getProperty( PROPERTY_PERIOD, org.kalypso.model.rcm.internal.binding.DateRange.class ).asDateRange( variables );
  }

  /**
   * @see org.kalypso.model.rcm.binding.IRainfallGenerator#getModels()
   */
  @Override
  public String[] getModels( )
  {
    String property = getProperty( PROPERTY_MODEL, String.class );
    if( property == null || property.length() == 0 )
      return new String[] {};

    return StringUtils.split( property, ";" );
  }
}