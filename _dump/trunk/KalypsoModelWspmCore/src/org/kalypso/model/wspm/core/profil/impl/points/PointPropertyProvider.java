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
package org.kalypso.model.wspm.core.profil.impl.points;

import java.util.HashMap;
import java.util.Map;

import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfilPointProperty;
import org.kalypso.model.wspm.core.profil.IProfilPointPropertyProvider;

/**
 * @author kimwerner
 */
public class PointPropertyProvider implements IProfilPointPropertyProvider
{
  private final Map<String, IProfilPointProperty> m_properties = new HashMap<String, IProfilPointProperty>();

  public PointPropertyProvider( )
  {
    m_properties.put( IWspmConstants.POINT_PROPERTY_BREITE, null );
    m_properties.put( IWspmConstants.POINT_PROPERTY_HOEHE, null );
    m_properties.put( IWspmConstants.POINT_PROPERTY_BEWUCHS_AX, null );
    m_properties.put( IWspmConstants.POINT_PROPERTY_BEWUCHS_AY, null );
    m_properties.put( IWspmConstants.POINT_PROPERTY_BEWUCHS_DP, null );
    m_properties.put( IWspmConstants.POINT_PROPERTY_HOCHWERT, null );
    m_properties.put( IWspmConstants.POINT_PROPERTY_RECHTSWERT, null );
    m_properties.put( IWspmConstants.POINT_PROPERTY_RAUHEIT, null );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilPointPropertyProvider#getPointProperties()
   * the order of these entries is the initial columnsort usesd in the tableview
   */
  public String[] getPointProperties( )
  {
    return new String[]{IWspmConstants.POINT_PROPERTY_BREITE,  IWspmConstants.POINT_PROPERTY_HOEHE,  IWspmConstants.POINT_PROPERTY_HOCHWERT,  IWspmConstants.POINT_PROPERTY_RECHTSWERT, IWspmConstants.POINT_PROPERTY_RAUHEIT ,IWspmConstants.POINT_PROPERTY_BEWUCHS_AX,  IWspmConstants.POINT_PROPERTY_BEWUCHS_AY, IWspmConstants.POINT_PROPERTY_BEWUCHS_DP};
  }

  private final IProfilPointProperty createPointProperty( final String property )
  {
    if( property.equals( IWspmConstants.POINT_PROPERTY_BREITE ) )
      return new PointProperty( property,"Breite", 0.0001, new String[] { IWspmConstants.POINT_PROPERTY_HOEHE }, false );
    if( property.equals( IWspmConstants.POINT_PROPERTY_HOEHE ) )
      return new PointProperty( property,"H�he", 0.0001, new String[] { IWspmConstants.POINT_PROPERTY_BREITE }, false );
    if( property.equals( IWspmConstants.POINT_PROPERTY_BEWUCHS_AX ) )
      return new PointProperty( property,"Bewuchs Ax", 0.0001, new String[] { IWspmConstants.POINT_PROPERTY_BEWUCHS_AY, IWspmConstants.POINT_PROPERTY_BEWUCHS_DP }, true );
    if( property.equals( IWspmConstants.POINT_PROPERTY_BEWUCHS_AY ) )
      return new PointProperty( property,"Bewuchs Ay", 0.0001, new String[] { IWspmConstants.POINT_PROPERTY_BEWUCHS_AX, IWspmConstants.POINT_PROPERTY_BEWUCHS_DP }, true );
    if( property.equals( IWspmConstants.POINT_PROPERTY_BEWUCHS_DP ) )
      return new PointProperty( property,"Bewuchs dP", 0.0001, new String[] { IWspmConstants.POINT_PROPERTY_BEWUCHS_AX, IWspmConstants.POINT_PROPERTY_BEWUCHS_AY }, true );
    if( property.equals( IWspmConstants.POINT_PROPERTY_RECHTSWERT ) )
      return new PointProperty( property,"Rechtswert", 0.0001, new String[] { IWspmConstants.POINT_PROPERTY_HOEHE}, true );
    if( property.equals( IWspmConstants.POINT_PROPERTY_HOCHWERT ) )
      return new PointProperty( property,"Hochwert", 0.0001, new String[] { IWspmConstants.POINT_PROPERTY_RECHTSWERT}, true );
    if( property.equals( IWspmConstants.POINT_PROPERTY_RAUHEIT ) )
      return new PointProperty( property,"Rauheit", 0.0001, new String[0] , false );

    return null;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilPointPropertyProvider#createPointProperty(java.lang.String)
   */
  public IProfilPointProperty getPointProperty( final String property )
  {
    IProfilPointProperty prop = m_properties.get( property );
    if( prop != null )
      return prop;
    if( m_properties.containsKey( property ) )
    {
      prop = createPointProperty( property );
      if( prop != null )
        m_properties.put( property, prop );
    }
    return prop;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilPointPropertyProvider#providesPointProperty(java.lang.String)
   */
  public boolean providesPointProperty( String property )
  {
    return m_properties.containsKey( property );
  }

}
