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
package org.kalypso.model.wspm.core.profil.impl.marker;

import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;

/**
 * @author kimwerner
 */
public class PointMarker implements IProfilPointMarker
{
  final private IComponent m_type;

  IRecord m_point = null;

  public PointMarker( final IComponent typ, final IRecord point )
  {
    m_type = typ;
    m_point = point;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilPointMarker#getId()
   */
  public IComponent getId( )
  {
    return m_type;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilPointMarker#getPoint()
   */
  public IRecord getPoint( )
  {
    return m_point;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilPointMarker#getValue()
   */
  public Object getValue( )
  {
    return m_point.getValue( getId() );
  }

  /* Interpreted ui values to obtain backward compability */
  public Object getIntepretedValue( )
  {

    return getValue();
  }

  public void setInterpretedValue( final Object value )
  {

    setValue( value );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilPointMarker#setPoint(org.kalypso.observation.result.IRecord)
   */
  public IRecord setPoint( final IRecord newPosition )
  {
    final IRecord oldPoint = m_point;
    if( m_point != null )
    {
      /*
       * get old value of point, change point mapping and set old value to new point and null old point value
       */
      final Object old = m_point.getValue( getId() );
      m_point.setValue( getId(), null );

      m_point = newPosition;
      m_point.setValue( getId(), old );
    }

    return oldPoint;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilPointMarker#setValue(java.lang.Object)
   */
  public void setValue( final Object value )
  {
    m_point.setValue( getId(), value );
  }
}
