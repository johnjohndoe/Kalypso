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
package org.kalypso.model.wspm.core.profil.impl.points;

import java.util.HashMap;

import org.kalypso.model.wspm.core.profil.IProfilPoint;

/**
 * @author kimwerner
 */
public class ProfilPoint implements IProfilPoint
{
  private final HashMap<String, Double> m_pointProperties = new HashMap<String, Double>();

  public ProfilPoint( )
  {

  }

  public ProfilPoint( final String[] pointProperties )
  {
    for( final String property : pointProperties )
    {
      m_pointProperties.put( property, 0.0 );
    }

  }

  public final void addProperty( final String pointProperty )
  {
    m_pointProperties.put( pointProperty, new Double( 0 ) );
  }

  /**
   * @return a copy of this point or null
   * @see org.kalypso.model.wspm.core.profilinterface.IProfilPoint#clonePoint()
   */
  public IProfilPoint clonePoint( )
  {
    final String[] properties = m_pointProperties.keySet().toArray( new String[0] );
    final ProfilPoint point = new ProfilPoint( properties );
    for( final String property : properties )
    {
      point.setValueFor( property, m_pointProperties.get( property ) );
    }
    return point;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilPoint#getProperties()
   */
  public String[] getProperties( )
  {
    return m_pointProperties.keySet().toArray( new String[0] );
  }

  public final double getValueFor( final String pointProperty ) throws IllegalArgumentException
  {
    if( !(m_pointProperties.containsKey( pointProperty )) )
      throw new IllegalArgumentException( "Profileigenschaft existiert nicht: " + pointProperty.toString() );
    return m_pointProperties.get( pointProperty ).doubleValue();
  }

  /**
   * @see org.kalypso.model.wspm.core.profilinterface.IProfilPoint#hasTableData(org.kalypso.model.wspm.core.profildata.tabledata.TableDataKey)
   */
  public boolean hasProperty( String pointProperty )
  {
    return m_pointProperties.containsKey( pointProperty );

  }

  public final void removeProperty( final String pointProperty )
  {
    m_pointProperties.remove( pointProperty );
  }

  public final boolean setValueFor( final String pointProperty, final double value )
  {
    if( !(m_pointProperties.containsKey( pointProperty )) )
      return false;
    m_pointProperties.put( pointProperty, new Double( value ) );
    return true;
  }

  

}
