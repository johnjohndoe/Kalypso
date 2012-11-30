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
package org.kalypso.ui.rrm.internal.conversion.to12_02;

import java.util.LinkedHashSet;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
import org.joda.time.Period;
import org.kalypso.model.hydrology.binding.timeseries.ITimeseries;
import org.kalypsodeegree.model.feature.IFeatureBindingCollectionVisitor;

import com.google.common.base.Objects;

/**
 * @author Dirk Kuch
 */
public class FindUniqueQualityVisitor implements IFeatureBindingCollectionVisitor<ITimeseries>
{
  Set<ITimeseries> m_timeserieses = new LinkedHashSet<>();

  private final String m_parameterType;

  private final Period m_timestep;

  public FindUniqueQualityVisitor( final String parameterType, final Period timestep )
  {
    m_parameterType = parameterType;
    m_timestep = timestep;
  }

  @Override
  public void visit( final ITimeseries timeseries )
  {
    if( !StringUtils.equals( timeseries.getParameterType(), m_parameterType ) )
      return;

    if( !Objects.equal( timeseries.getTimestep(), m_timestep ) )
      return;

    m_timeserieses.add( timeseries );
  }

  public boolean hasQuality( final String quality )
  {
    for( final ITimeseries timeseries : m_timeserieses )
    {
      if( StringUtils.equalsIgnoreCase( timeseries.getQuality(), quality ) )
        return true;
    }

    return false;
  }

}
