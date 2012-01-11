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

import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.TreeSet;

import org.apache.commons.lang3.ArrayUtils;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.commons.java.lang.Arrays;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.core.profil.base.FLOW_DIRECTION;
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
  private final Set<KnaufProfileWrapper> m_profiles;

  private FLOW_DIRECTION m_direction;

  public KnaufReach( final IProfileFeature[] profiles )
  {
    m_profiles = new TreeSet<>( new ProfileFeatureStationComparator( getDirection( profiles ).isUpstream() ) );
    for( final IProfileFeature profile : profiles )
    {
      m_profiles.add( new KnaufProfileWrapper( this, profile.getProfil() ) );
    }
  }

  public void addProfiles( final KnaufProfileWrapper... profiles )
  {
    if( Arrays.isEmpty( profiles ) )
      return;

    for( final KnaufProfileWrapper profile : profiles )
    {
      m_profiles.add( profile );
    }
  }

  private FLOW_DIRECTION getDirection( final IProfileFeature[] profiles )
  {
    for( final IProfileFeature profile : profiles )
    {
      final Feature parent = profile.getOwner();

      if( parent instanceof WspmWaterBody )
      {
        final WspmWaterBody waterBody = (WspmWaterBody) parent;
        {
          m_direction = FLOW_DIRECTION.toFlowDirection( waterBody.isDirectionUpstreams() );
          return m_direction;
        }
      }
    }

    throw new UnsupportedOperationException();
  }

  public FLOW_DIRECTION getDirection( )
  {
    return m_direction;
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
      final KnaufProfileBeanBuilder builder = new KnaufProfileBeanBuilder( this, profile );
      final IStatus status = builder.execute( new NullProgressMonitor() ); // TODO

      Collections.addAll( beans, builder.getBeans() );
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
    return m_profiles.toArray( new KnaufProfileWrapper[] {} );
  }

  public KnaufProfileWrapper findNextProfile( final KnaufProfileWrapper profile )
  {
    final KnaufProfileWrapper[] profiles = getProfiles();

    final int index = ArrayUtils.indexOf( profiles, profile );
    if( index < 0 )
      return null;

    if( org.kalypso.commons.java.lang.Arrays.isLastItem( profiles, profile ) )
      return null;

    return profiles[index + 1];
  }

}
