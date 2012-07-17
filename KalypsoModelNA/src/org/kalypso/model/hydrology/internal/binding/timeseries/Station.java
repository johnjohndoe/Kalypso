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
package org.kalypso.model.hydrology.internal.binding.timeseries;

import java.util.LinkedHashSet;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
import org.joda.time.Period;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.binding.timeseries.IStation;
import org.kalypso.model.hydrology.binding.timeseries.ITimeseries;
import org.kalypso.model.hydrology.binding.timeseries.StationUtils;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.feature.FeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;

/**
 * @author Gernot Belger
 */
public abstract class Station extends Feature_Impl implements IStation
{
  private final IFeatureBindingCollection<ITimeseries> m_timeseries = new FeatureBindingCollection<>( this, ITimeseries.class, MEMBER_TIMESERIES );

  public Station( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  @Override
  public IFeatureBindingCollection<ITimeseries> getTimeseries( )
  {
    return m_timeseries;
  }

  @Override
  public String getComment( )
  {
    return getProperty( PROPERTY_COMMENT, String.class );
  }

  @Override
  public void setComment( final String comment )
  {
    setProperty( PROPERTY_COMMENT, comment );
  }

  @Override
  public String getGroup( )
  {
    final String group = getProperty( PROPERTY_GROUP, String.class );

    // Treat all blank names as the same, empty group
    if( StringUtils.isBlank( group ) )
      return null;

    return group;
  }

  @Override
  public void setGroup( final String groupName )
  {
    setProperty( PROPERTY_GROUP, groupName );
  }

  @Override
  public String getTimeseriesFoldername( )
  {
    // REMARK: delegate to static method, to ensure this logic is only in one place
    return StationUtils.getTimeseriesFoldername( getDescription() );
  }

  @Override
  public GM_Point getStationLocation( )
  {
    return getProperty( PROPERTY_LOCATION, GM_Point.class );
  }

  @Override
  public boolean hasTimeseries( final String parameterType, final String quality, final Period timestep )
  {
    final IFeatureBindingCollection<ITimeseries> timeserieses = getTimeseries();
    for( final ITimeseries timeseries : timeserieses )
    {
      if( parameterType.equals( timeseries.getParameterType() ) && timestep.equals( timeseries.getTimestep() ) )
      {
        if( quality.equalsIgnoreCase( timeseries.getQuality() ) )
          return true;
      }
    }

    return false;
  }

  @Override
  public String[] getQualities( final String parameterType )
  {
    final Set<String> qualities = new LinkedHashSet<>();

    final IFeatureBindingCollection<ITimeseries> timeserieses = getTimeseries();
    for( final ITimeseries ts : timeserieses )
    {
      final String quality = ts.getQuality();
      final String tsType = ts.getParameterType();

      if( parameterType == null || tsType.equals( parameterType ) )
      {
        if( quality != null )
          qualities.add( quality );
        else
          qualities.add( StringUtils.EMPTY );
      }
    }

    return qualities.toArray( new String[] {} );
  }
}