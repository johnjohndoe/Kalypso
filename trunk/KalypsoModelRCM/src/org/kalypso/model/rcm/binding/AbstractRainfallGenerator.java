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

import org.apache.commons.lang3.StringUtils;
import org.kalypso.commons.tokenreplace.IStringResolver;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.zml.core.filter.binding.IZmlFilter;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;

/**
 * @author Gernot Belger
 */
public abstract class AbstractRainfallGenerator extends Feature_Impl implements IRainfallGenerator
{
  protected AbstractRainfallGenerator( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  @Override
  public IDateRange getPeriod( )
  {
    return getProperty( PROPERTY_PERIOD, IDateRange.class );
  }

  @Override
  public DateRange getPeriod( final IStringResolver variables )
  {
    final IDateRange property = getProperty( PROPERTY_PERIOD, IDateRange.class );
    if( property == null )
      return null;

    return property.asDateRange( variables );
  }

  @Override
  public void setPeriod( final DateRange period )
  {
    final IRelationType relation = (IRelationType) getFeatureType().getProperty( PROPERTY_PERIOD );
    final IFeatureType type = GMLSchemaUtilities.getFeatureTypeQuiet( IDateRange.FEATURE_DATE_RANGE );
    final org.kalypso.model.rcm.internal.binding.DateRange periodFeature = (org.kalypso.model.rcm.internal.binding.DateRange) getWorkspace().createFeature( this, relation, type );

    periodFeature.setFrom( period.getFrom() );
    periodFeature.setTo( period.getTo() );

    setProperty( PROPERTY_PERIOD, periodFeature );
  }

  @Override
  public String[] getModels( )
  {
    final String property = getProperty( PROPERTY_MODEL, String.class );
    if( StringUtils.isBlank( property ) )
      return new String[] {};

    return StringUtils.split( property, ";" );
  }

  @Override
  public String getParameterType( )
  {
    return getProperty( PROPERTY_PARAMETER_TYPE, String.class );
  }

  /**
   * @see org.kalypso.model.rcm.binding.IRainfallGenerator#addFilter(org.kalypso.zml.core.filter.binding.IZmlFilter)
   */
  @Override
  public void addFilter( final IZmlFilter filter )
  {
    // TODO
  }
}