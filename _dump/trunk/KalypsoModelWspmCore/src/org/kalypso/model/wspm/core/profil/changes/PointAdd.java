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

import org.apache.commons.lang.ArrayUtils;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.observation.result.IRecord;

/**
 * @author kimwerner
 */
public class PointAdd implements IProfilChange
{
  private final IProfil m_profil;

  private final IRecord m_pointBefore;

  private final IRecord m_point;

  public PointAdd( final IProfil profil, final IRecord pointBefore, final IRecord point )
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
    IRecord pointToAdd = null;
    if( m_point != null )
      pointToAdd = m_point;
    else if( m_pointBefore != null )
      pointToAdd = m_pointBefore.cloneRecord();
    if( pointToAdd == null )
      return new IllegalChange( "Profilpunkt existiert nicht.", this );

    final IRecord[] points = m_profil.getRecordPoints();

    if( m_pointBefore == null )
    {
      m_profil.getResult().add( 0, m_point );

    }
    else
    {
      final int index = ArrayUtils.indexOf( points, m_pointBefore );
      if( index < 0 )
        return new IllegalChange( "Profilpunkt existiert nicht.", this );

      m_profil.getResult().add( index + 1, pointToAdd );
    }
    return new PointRemove( m_profil, pointToAdd );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilChange#getObject()
   */
  public Object[] getObjects( )
  {
    return new IRecord[] { m_point };
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
