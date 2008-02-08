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
package org.kalypso.model.wspm.tuhh.core.profile;

import java.util.LinkedHashSet;
import java.util.Set;

import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfilPointPropertyProvider;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.observation.phenomenon.DictionaryPhenomenon;
import org.kalypso.observation.result.Component;
import org.kalypso.observation.result.IComponent;

/**
 * @author kimwerner
 */
public class PointPropertyProviderTUHH implements IProfilPointPropertyProvider
{
  final Set<IComponent> m_properties = new LinkedHashSet<IComponent>();

  public PointPropertyProviderTUHH( )
  {
    m_properties.add( createPointProperty( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE ) );
    m_properties.add( createPointProperty( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR ) );
    m_properties.add( createPointProperty( IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE ) );

    // TODO Markers
    m_properties.add( createPointProperty( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ) );
    m_properties.add( createPointProperty( IWspmTuhhConstants.MARKER_TYP_BORDVOLL ) );
    m_properties.add( createPointProperty( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE ) );
    m_properties.add( createPointProperty( IWspmTuhhConstants.MARKER_TYP_WEHR ) );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilPointPropertyProvider#getPointProperties()
   */
  public IComponent[] getPointProperties( )
  {
    return m_properties.toArray( new IComponent[] {} );
  }

  public static final IComponent createPointProperty( final String property )
  {
    if( property.equals( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE ) )
      return new Component( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE, "Oberkante Br�cke", "Oberkante Br�cke", "", "", IWspmConstants.Q_DOUBLE, Double.NaN, new DictionaryPhenomenon( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE, "Oberkante Br�cke", "Oberkante Br�cke" ) );
    else if( property.equals( IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE ) )
      return new Component( IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE, "Unterkante Br�cke", "Unterkante Br�cke", "", "", IWspmConstants.Q_DOUBLE, Double.NaN, new DictionaryPhenomenon( IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE, "Unterkante Br�cke", "Unterkante Br�cke" ) );
    else if( property.equals( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR ) )
      return new Component( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR, "Oberkante Wehr", "Oberkante Wehr", "", "", IWspmConstants.Q_DOUBLE, Double.NaN, new DictionaryPhenomenon( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR, "Oberkante Wehr", "Oberkante Wehr" ) );
    else if( property.equals( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ) )
      return new Component( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE, "Trennflaeche", "Trennflaeche", "", "", IWspmConstants.Q_STRING, "", new DictionaryPhenomenon( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE, "Trennfl�che", "Trennfl�che" ) );
    else if( property.equals( IWspmTuhhConstants.MARKER_TYP_BORDVOLL ) )
      return new Component( IWspmTuhhConstants.MARKER_TYP_BORDVOLL, "Bordvoll", "Bordvoll", "", "", IWspmConstants.Q_BOOLEAN, null, new DictionaryPhenomenon( IWspmTuhhConstants.MARKER_TYP_BORDVOLL, "Bordvoll", "Bordvoll" ) );
    else if( property.equals( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE ) )
      return new Component( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE, "Durchstr�mter Bereich", "Durchstr�mter Bereich", "", "", IWspmConstants.Q_BOOLEAN, null, new DictionaryPhenomenon( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE, "Durchstr�mter Bereich", "Duchstr�mter Bereich" ) );
    else if( property.equals( IWspmTuhhConstants.MARKER_TYP_WEHR ) )
      return new Component( IWspmTuhhConstants.MARKER_TYP_WEHR, "Wehrfeldtrenner", "Wehrfeldtrenner", "", "", IWspmConstants.Q_DOUBLE, 0.0, new DictionaryPhenomenon( IWspmTuhhConstants.MARKER_TYP_WEHR, "Wehrfeldtrenner", "Wehrfeldtrenner" ) );

    throw new IllegalStateException( "property not defined" );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilPointPropertyProvider#providesPointProperty(java.lang.String)
   */
  public boolean providesPointProperty( final String profilPointProperty )
  {
    for( final IComponent component : m_properties )
    {
      if( component.getId().equals( profilPointProperty ) )
        return true;
    }

    return false;
  }

  public IComponent getPointProperty( final String propertyId )
  {
    for( final IComponent component : m_properties )
    {
      if( component.getId().equals( propertyId ) )
        return component;
    }

    return null;
  }

}
