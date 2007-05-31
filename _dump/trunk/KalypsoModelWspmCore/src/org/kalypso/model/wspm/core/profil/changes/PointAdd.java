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
package org.kalypso.model.wspm.core.profil.changes;

import java.util.LinkedList;

import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilPoint;

/**
 * @author kimwerner
 */
public class PointAdd implements IProfilChange
{
  private final IProfil m_profil;

  private final IProfilPoint m_pointBefore;

  private IProfilPoint m_point;

  public PointAdd( final IProfil profil, final IProfilPoint pointBefore, final IProfilPoint point )
  {
    m_profil = profil;
    m_pointBefore = pointBefore;
    m_point = point;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilChange#doChange()
   */
  public IProfilChange doChange( final ProfilChangeHint hint )
  {

    if( hint != null )
    {
      hint.setPointsChanged();
    }
    IProfilPoint pointToAdd=null;
    if( m_point != null )
      pointToAdd = m_point;
    else if( m_pointBefore != null )
      pointToAdd = m_pointBefore.clonePoint();
    if( pointToAdd == null )
      return new IllegalChange( "Profilpunkt existiert nicht.",this );
    final LinkedList<IProfilPoint> points = m_profil.getPoints();
    if( m_pointBefore == null )
    {
      points.addFirst( m_point );
    }
    else
    {
      final int i = points.indexOf( m_pointBefore );
      if( i < 0 )
        return new IllegalChange( "Profilpunkt existiert nicht.",this );
      points.add( i + 1, pointToAdd );
    }
    return new PointRemove( m_profil, pointToAdd );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilChange#getObject()
   */
  public Object[] getObjects( )
  {
    return new IProfilPoint[]{m_point};
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilChange#getPointProperty()
   */
  public String getInfo( )
  {
    return null;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilChange#getValue()
   */
  public Double getValue( )
  {
    return null;
  }
}
