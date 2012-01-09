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
package org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans;

import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.KnaufReach;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.base.KnaufProfilePointWrapper;

/**
 * @author Dirk Kuch
 */
public class KnaufSA21Bean extends AbstractKnaufProjectBean
{
  private final KnaufReach m_reach;

  private final IProfileFeature m_profile;

  private Double m_localVerlustBeiwertZeta = 0.0;

  public KnaufSA21Bean( final KnaufReach reach, final IProfileFeature profile )
  {
    m_reach = reach;
    m_profile = profile;
  }

  @Override
  public Integer getSatzart( )
  {
    return 21;
  }

  /**
   * @return profile station in m
   */
  public Double getStation( )
  {
    return m_profile.getStation() * 1000.0;
  }

  public Double getRightForelandRoughness( )
  {
    final KnaufProfilePointWrapper profile = new KnaufProfilePointWrapper( m_profile.getProfil() );

    return profile.getRightForelandRoughness();
  }

  public Double getRiverbedRoughness( )
  {
    final KnaufProfilePointWrapper profile = new KnaufProfilePointWrapper( m_profile.getProfil() );

    return profile.getRiverbedRoughness();
  }

  public Double getLeftForelandRoughness( )
  {
    final KnaufProfilePointWrapper profile = new KnaufProfilePointWrapper( m_profile.getProfil() );

    return profile.getLeftForelandRoughness();
  }

  /**
   * 46 A1 ABSZETA bei ABSZETA = A wird ZETA als absolute Verlusthöhe in m eingesetzt <br>
   * ABSZETA = S : Manning-Strickler<br>
   * ABSZETA = P : Prandtl-Colebrook
   */
  public char getABSZeta( )
  {
    return 'S'; // FIXME
  }

  public Double getLocalVerlustBeiwertZeta( )
  {
    return m_localVerlustBeiwertZeta;
  }

  public void setLocalVerlustBeiwertZeta( final Double localVerlustBeiwertZeta )
  {
    m_localVerlustBeiwertZeta = localVerlustBeiwertZeta;
  }

}
