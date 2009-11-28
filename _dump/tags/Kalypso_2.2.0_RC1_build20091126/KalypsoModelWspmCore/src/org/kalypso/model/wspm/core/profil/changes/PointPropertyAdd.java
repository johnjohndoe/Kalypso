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
package org.kalypso.model.wspm.core.profil.changes;

import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;

public final class PointPropertyAdd implements IProfilChange
{
  private final IProfil m_profil;

  private final IComponent m_property;

  private final Object[] m_values;

  private final IComponent m_cloneFrom;

  public PointPropertyAdd( final IProfil profil, final IComponent property, final Object[] values )
  {
    m_profil = profil;
    m_property = property;
    m_values = values;
    m_cloneFrom = null;
  }

  public PointPropertyAdd( final IProfil profil, final IComponent property, final IComponent component )
  {
    m_profil = profil;
    m_property = property;
    m_values = null;
    m_cloneFrom = component;
  }
  public PointPropertyAdd( final IProfil profil, final IComponent property)
  {
    m_profil = profil;
    m_property = property;
    m_cloneFrom = null;
    m_values = null;
  }
  public PointPropertyAdd( final IProfil profil, final IComponent property, final Object defaultValue )
  {
    m_profil = profil;
    m_property = property;
    m_cloneFrom = null;
    m_values =  new Object[]{defaultValue};
  }

  public IProfilChange doChange( final ProfilChangeHint hint )
  {
    if( hint != null )
      hint.setPointPropertiesChanged();
    if( m_cloneFrom != null )
      m_profil.getResult().addComponent( m_property,m_cloneFrom );
    else if (m_values==null)
      m_profil.addPointProperty( m_property );
    else if (m_values.length==1)
      m_profil.addPointProperty( m_property ,m_values[0]);
    else
    {
       m_profil.addPointProperty( m_property );
       final int index = m_profil.getResult().indexOfComponent( m_property );
      final IRecord[] points = m_profil.getPoints();
      if( m_values != null && points.length == m_values.length )
      {
        int i = 0;
        for( final IRecord point : points )
          point.setValue( index, m_values[i++] );
      }
    }
    return new PointPropertyRemove( m_profil, m_property );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilChange#getObject()
   */
  public Object[] getObjects( )
  {
    return m_values;
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
    return null;
  }

}