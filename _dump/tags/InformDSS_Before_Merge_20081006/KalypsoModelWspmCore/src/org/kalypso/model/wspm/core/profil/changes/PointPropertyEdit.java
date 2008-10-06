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

import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;

public final class PointPropertyEdit implements IProfilChange
{
  private final IRecord[] m_points;

  private final IComponent m_property;

  private final Object[] m_newValues;

  public PointPropertyEdit( final IRecord p, final IComponent property, final Object newValue )
  {
    m_points = new IRecord[] { p };
    m_property = property;
    m_newValues = new Object[] { newValue };
  }

  public PointPropertyEdit( final IRecord[] points, final IComponent property, final Object[] newValues )
  {
    m_points = points;
    m_property = property;
    m_newValues = newValues;
  }

  /**
   * @throws IllegalProfileOperationException
   * @see org.kalypso.model.wspm.core.profil.changes.AbstractChange#doChange(PlainProfil)
   */
  public IProfilChange doChange( final ProfilChangeHint hint )
  {
    if( hint != null )
      hint.setPointValuesChanged();
    final Object[] oldValues = new Object[m_points.length];
    int i = 0;
    for( final IRecord point : m_points )
    {
      if( i < oldValues.length )
      {
        oldValues[i] = point.getValue( m_property );

        point.setValue( m_property, i < m_newValues.length ? m_newValues[i] : Double.NaN );
      }
      i++;
    }
    return new PointPropertyEdit( m_points, m_property, oldValues );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilChange#getObject()
   */
  public Object[] getObjects( )
  {
    return m_points;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilChange#getPointProperty()
   */
  public String getInfo( )
  {
    return m_property.toString();
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilChange#getValue()
   */
  public Double getValue( )
  {
    return Double.NaN;
  }

}