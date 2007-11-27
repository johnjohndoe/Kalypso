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
package org.kalypso.model.wspm.tuhh.core.profile.buildings;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;

import org.kalypso.model.wspm.core.profil.IProfileObject;

/**
 * @author kimwerner
 */
public abstract class AbstractBuilding implements IProfileObject
{

  private String m_name;

  private final HashMap<String, String> m_labels = new HashMap<String, String>();

  protected final String m_buildingTyp;

  protected final Map<String, Object> m_buildingValues = new LinkedHashMap<String, Object>();

  public AbstractBuilding( final String buildingTyp, final String name, final String[] properties, final String[] labels )
  {
    m_buildingTyp = buildingTyp;
    m_name = name;
    for( int i = 0; i < properties.length; i++ )
    {
      m_buildingValues.put( properties[i], Double.NaN );
      m_labels.put( properties[i], i < labels.length ? labels[i] : "" );
    }
  }

  /**
   * @return Returns the buildingTyp.
   */
  public String getId( )
  {
    return m_buildingTyp;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfileObject#getName()
   */
  public String getName( )
  {
    return m_name;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfileObject#getBuildingProperties()
   */
  public String[] getObjectProperties( )
  {
    return m_buildingValues.keySet().toArray( new String[0] );
  }

  /**
   * @see org.kalypso.model.wspm.core.profilinterface.IProfilBuilding#getValue(org.kalypso.model.wspm.core.profilinterface.IProfil.BUILDING_PROPERTY,
   *      TYPE) return maybe Null
   */
  public Object getValueFor( String buildingValue )
  {
    if( m_buildingValues.containsKey( buildingValue ) )
      return m_buildingValues.get( buildingValue );
    throw new IllegalArgumentException( "Die Eigenschaft " + buildingValue + " wird von diesem Bauwerk nicht unterst¸tzt." );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfileObject#hasProperty(org.kalypso.model.wspm.core.profil.BUILDING_PROPERTY)
   */
  public boolean hasProperty( String profilBuildingProperty )
  {
    return m_buildingValues.containsKey( profilBuildingProperty );
  }

  /**
   * @return oldvalue maybe Null
   * @see org.kalypso.model.wspm.core.profilinterface.IProfilBuilding#setValue(org.kalypso.model.wspm.core.profilinterface.IProfil.BUILDING_PROPERTY,
   *      Object)
   */
  public Object setValue( final String property, final Object value ) throws IllegalArgumentException
  {
    if( !m_buildingValues.containsKey( property ) )
      throw new IllegalArgumentException( "Die Eigenschaft " + property + " wird von diesem Bauwerk nicht unterst¸tzt." );

    final Object oldValue = m_buildingValues.get( property );

    m_buildingValues.put( property, value );

    return oldValue;
  }

  public final String getLabelFor( final String profilBuildingProperty )
  {
    return m_labels.containsKey( profilBuildingProperty ) ? m_labels.get( profilBuildingProperty ) : "";
  }
}
