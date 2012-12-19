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
package org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans;

import org.apache.commons.lang3.ArrayUtils;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.model.wspm.core.profil.visitors.ProfileVisitors;
import org.kalypso.model.wspm.core.profil.wrappers.IProfileRecord;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.base.KnaufProfileWrapper;

/**
 * @author Dirk Kuch
 */
public class KnaufSA20Bean extends AbstractKnaufProjectBean
{

  private final KnaufProfileWrapper m_profile;

  private Double m_pfeilerFormBeiwert;

  public KnaufSA20Bean( final KnaufProfileWrapper profile )
  {
    m_profile = profile;
  }

  public KnaufProfileWrapper getProfile( )
  {
    return m_profile;
  }

  @Override
  public Integer getSatzart( )
  {
    return 20;
  }

  /**
   * @return profile station in m
   */
  public Double getStation( )
  {
    return m_profile.getStation() * 1000.0;
  }

  public Integer getNumberOfProfilePoints( )
  {
    return ArrayUtils.getLength( m_profile.getProfile().getPoints() );
  }

  public Double getDistanceNextProfile( )
  {
    final KnaufProfileWrapper next = m_profile.findNextProfile();
    if( Objects.isNull( next ) )
      return 0.0;

    final double distance = Math.abs( m_profile.getStation() - next.getStation() );

    return distance * 1000.0; // distance in m
  }

  public IProfileRecord findLowestPoint( )
  {
    return ProfileVisitors.findLowestPoint( m_profile.getProfile() );
  }

  public Double getPfeilerFormBeiwert( )
  {
    return m_pfeilerFormBeiwert;
  }

  public void setPfeilerFormBeiwert( final Double pfeilerFormBeiwert )
  {
    m_pfeilerFormBeiwert = pfeilerFormBeiwert;
  }
}
