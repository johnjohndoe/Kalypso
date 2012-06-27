/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.model.wspm.tuhh.core.profile.export.knauf.base;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.wrappers.IProfileRecord;
import org.kalypso.model.wspm.core.profil.wrappers.ProfileWrapper;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.KnaufReach;
import org.kalypso.observation.result.IComponent;

/**
 * @author Dirk Kuch
 */
public class KnaufProfileWrapper extends ProfileWrapper
{

  private final KnaufReach m_reach;

  public KnaufProfileWrapper( final KnaufReach reach, final IProfil profile )
  {
    super( profile );

    m_reach = reach;
  }

  public Double getRightForelandRoughness( )
  {
    final IProfilPointMarker[] trennflaechen = getProfilePointMarkerWrapper( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
    final IProfilPointMarker[] durchstroemt = getProfilePointMarkerWrapper( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE );

    if( ArrayUtils.isEmpty( trennflaechen ) || ArrayUtils.isEmpty( durchstroemt ) )
      return 0.0;

    final IProfilPointMarker w1 = findMax( trennflaechen );
    final IProfilPointMarker w2 = findMax( durchstroemt );

    final CalculateRoughenessVisitor visitor = new CalculateRoughenessVisitor( Math.min( w1.getPoint().getBreite(), w2.getPoint().getBreite() ), Math.max( w1.getPoint().getBreite(), w2.getPoint().getBreite() ) );
    getProfile().accept( visitor, 1 );

    return visitor.getRoughness( m_reach );
  }

  public Double getRiverbedRoughness( )
  {
    final IProfilPointMarker[] trennflaechen = getProfilePointMarkerWrapper( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
    final IProfilPointMarker w1 = findMin( trennflaechen );
    final IProfilPointMarker w2 = findMax( trennflaechen );

    final CalculateRoughenessVisitor visitor = new CalculateRoughenessVisitor( Math.min( w1.getPoint().getBreite(), w2.getPoint().getBreite() ), Math.max( w1.getPoint().getBreite(), w2.getPoint().getBreite() ) );
    getProfile().accept( visitor, 1 );

    return visitor.getRoughness( m_reach );
  }

  private IProfilPointMarker findMin( final IProfilPointMarker[] wrappers )
  {
    IProfilPointMarker ptr = null;
    for( final IProfilPointMarker wrapper : wrappers )
    {
      if( Objects.isNull( ptr ) )
        ptr = wrapper;
      else if( ptr.getPoint().getBreite() > wrapper.getPoint().getBreite() )
        ptr = wrapper;
    }

    return ptr;
  }

  private IProfilPointMarker findMax( final IProfilPointMarker[] wrappers )
  {
    IProfilPointMarker ptr = null;
    for( final IProfilPointMarker wrapper : wrappers )
    {
      if( Objects.isNull( ptr ) )
        ptr = wrapper;
      else if( ptr.getPoint().getBreite() < wrapper.getPoint().getBreite() )
        ptr = wrapper;
    }

    return ptr;
  }

  public Double getLeftForelandRoughness( )
  {
    final IProfilPointMarker[] trennflaechen = getProfilePointMarkerWrapper( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
    final IProfilPointMarker[] durchstroemt = getProfilePointMarkerWrapper( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE );

    if( ArrayUtils.isEmpty( trennflaechen ) || ArrayUtils.isEmpty( durchstroemt ) )
      return 0.0;

    final IProfilPointMarker w1 = findMin( trennflaechen );
    final IProfilPointMarker w2 = findMin( durchstroemt );

    final CalculateRoughenessVisitor visitor = new CalculateRoughenessVisitor( Math.min( w1.getPoint().getBreite(), w2.getPoint().getBreite() ), Math.max( w1.getPoint().getBreite(), w2.getPoint().getBreite() ) );
    getProfile().accept( visitor, 1 );

    return visitor.getRoughness( m_reach );
  }

  public String getKennziffer( final IProfileRecord point )
  {
    final IProfilPointMarker[] markers = getPointMarkers( point );
    if( ArrayUtils.isEmpty( markers ) )
      return StringUtils.repeat( " ", 2 ); //$NON-NLS-1$

    for( final IProfilPointMarker marker : markers )
    {
      final IComponent component = marker.getComponent();
      final String identifier = component.getId();

      if( StringUtils.equals( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE, identifier ) )
      {
        if( isLeftMarker( marker, identifier ) )
        {
          return "PA";//$NON-NLS-1$
        }

        return "PE";//$NON-NLS-1$
      }
// else if( StringUtils.equals( IWspmTuhhConstants.MARKER_TYP_BORDVOLL, identifier ) )
// {
// if( isLeftMarker( marker, identifier ) )
// {
//          return "LB";//$NON-NLS-1$
// }
//
//        return "RB"; //$NON-NLS-1$
// }
      else if( StringUtils.equals( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE, identifier ) )
      {
        if( isLeftMarker( marker, identifier ) )
        {
          return "LU";//$NON-NLS-1$
        }

        return "RU";//$NON-NLS-1$
      }
    }

    return StringUtils.repeat( " ", 2 ); //$NON-NLS-1$
  }

  private boolean isLeftMarker( final IProfilPointMarker marker, final String identifier )
  {
    double ptr = Double.MAX_VALUE;

    final IProfilPointMarker[] markers = getProfilePointMarkerWrapper( identifier );
    for( final IProfilPointMarker m : markers )
    {
      if( m.getPoint().getBreite() < ptr )
        ptr = m.getPoint().getBreite();
    }

    return marker.getPoint().getBreite() == ptr;
  }

  public String getAusuferungsgrenze( final IProfileRecord point )
  {
    final IProfilPointMarker[] markers = getPointMarkers( point );
    if( ArrayUtils.isEmpty( markers ) )
      return " "; //$NON-NLS-1$

    for( final IProfilPointMarker marker : markers )
    {
      final IComponent component = marker.getComponent();
      final String identifier = component.getId();

      if( StringUtils.equals( IWspmTuhhConstants.MARKER_TYP_BORDVOLL, identifier ) )
      {
        if( isLeftMarker( marker, identifier ) )
        {
          return "L";//$NON-NLS-1$
        }

        return "R"; //$NON-NLS-1$
      }
    }

    return " "; //$NON-NLS-1$
  }

  public KNAUF_FLIESSGESETZ getFliessgesetz( )
  {
    return m_reach.getFliessgesetz();
  }

  public KnaufReach getReach( )
  {
    return m_reach;
  }

  public KnaufProfileWrapper findNextProfile( )
  {
    return getReach().findNextProfile( this );
  }

}
