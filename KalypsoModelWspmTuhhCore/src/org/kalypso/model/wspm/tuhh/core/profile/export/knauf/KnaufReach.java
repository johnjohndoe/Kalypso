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
package org.kalypso.model.wspm.tuhh.core.profile.export.knauf;

import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.Set;

import org.apache.commons.lang3.ArrayUtils;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.tuhh.core.gml.ProfileFeatureStationComparator;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.base.KNAUF_FLIESSGESETZ;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.base.KnaufProfileWrapper;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans.AbstractKnaufProjectBean;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans.KnaufProfileBeanBuilder;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans.KnaufSA14Bean;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans.KnaufSA15Bean;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans.KnaufSA16Bean;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans.KnaufSA40Bean;
import org.kalypsodeegree.model.feature.Feature;

/**
 * First basic implementation of a KnaufReach. Implementation will be analog to
 * {@link org.kalypso.model.wspm.tuhh.core.gml.TuhhReach}
 * 
 * @author Dirk Kuch
 */
public class KnaufReach
{

  private final KnaufProfileWrapper[] m_profiles;

  public KnaufReach( final IProfileFeature[] profiles )
  {

    final ProfileFeatureStationComparator comparator = new ProfileFeatureStationComparator( getDirection( profiles ) );
    Arrays.sort( profiles, comparator );

    final Set<KnaufProfileWrapper> wrappers = new LinkedHashSet<>();

    for( final IProfileFeature profile : profiles )
    {
      wrappers.add( new KnaufProfileWrapper( this, profile ) );
    }

    m_profiles = wrappers.toArray( new KnaufProfileWrapper[] {} );
  }

  private boolean getDirection( final IProfileFeature[] profiles )
  {

    for( final IProfileFeature profile : profiles )
    {
      final Feature parent = profile.getOwner();

      if( parent instanceof WspmWaterBody )
      {
        final WspmWaterBody waterBody = (WspmWaterBody) parent;
        return waterBody.isDirectionUpstreams();
      }
    }

    // TODO
    throw new UnsupportedOperationException();
  }

  public AbstractKnaufProjectBean[] toBeans( )
  {
    final Set<AbstractKnaufProjectBean> beans = new LinkedHashSet<>();
    beans.add( new KnaufSA14Bean( this ) );
    beans.add( new KnaufSA15Bean( this ) );
    beans.add( new KnaufSA16Bean( this ) );

    final KnaufProfileWrapper[] profiles = getProfiles();
    for( final KnaufProfileWrapper profile : profiles )
    {
      Collections.addAll( beans, KnaufProfileBeanBuilder.toBeans( this, profile ) );
    }

    beans.add( new KnaufSA40Bean( this ) );

    return beans.toArray( new AbstractKnaufProjectBean[] {} );
  }

  public KNAUF_FLIESSGESETZ getFliessgesetz( )
  {
    return KNAUF_FLIESSGESETZ.eManningStrickler; // TODO
  }

  public KnaufProfileWrapper[] getProfiles( )
  {
    return m_profiles;
  }

  public KnaufProfileWrapper findNextProfile( final KnaufProfileWrapper profile )
  {
    final int index = ArrayUtils.indexOf( m_profiles, profile );
    if( index < 0 )
      return null;

    if( org.kalypso.commons.java.lang.Arrays.isLastItem( m_profiles, profile ) )
      return null;

    return m_profiles[index + 1];
  }

}
