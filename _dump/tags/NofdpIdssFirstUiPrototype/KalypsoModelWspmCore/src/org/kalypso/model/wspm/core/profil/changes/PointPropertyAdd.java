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
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;


public final class PointPropertyAdd implements IProfilChange
{
  private final IProfil m_profil;

  private final POINT_PROPERTY m_property;

  private final double[] m_values;

  public PointPropertyAdd( final IProfil profil, final POINT_PROPERTY property,
      final double[] values )
  {
    m_profil = profil;
    m_property = property;
    m_values = values;
  }

  public PointPropertyAdd( final IProfil profil, final POINT_PROPERTY property,
      final double defaultValue )
  {
    m_profil = profil;
    m_property = property;
    m_values = new double[profil.getPoints().size()];
    for( int i = 0; i < m_values.length; i++ )
    {
      m_values[i] = defaultValue;
    }
  }

  public IProfilChange doChange( final ProfilChangeHint hint )
  {
    if (hint!=null) hint.setPointPropertiesChanged();
    
    m_profil.getProfilPoints().addProperty( m_property );
    final LinkedList<IProfilPoint> points = m_profil.getPoints();
    int i = 0;
    for( IProfilPoint point : points )
    {
      point.setValueFor( m_property, m_values[i++] );

    }
    return new PointPropertyRemove( m_profil, m_property );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilChange#getObject()
   */
  public Object getObject( )
  {
        return m_values;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilChange#getPointProperty()
   */
  public POINT_PROPERTY getPointProperty( )
  {
    return m_property;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilChange#getValue()
   */
  public Double getValue( )
  {
       return null;
  }

  
}