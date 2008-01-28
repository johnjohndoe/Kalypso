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
package org.kalypso.model.wspm.core.profil.impl.points;

import java.util.LinkedHashSet;
import java.util.Set;

import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfilPointPropertyProvider2;
import org.kalypso.observation.phenomenon.DictionaryPhenomenon;
import org.kalypso.observation.result.Component;
import org.kalypso.observation.result.IComponent;

/**
 * @author kimwerner
 */
public class PointPropertyProvider implements IProfilPointPropertyProvider2
{
  private final Set<IComponent> m_properties = new LinkedHashSet<IComponent>();

  public PointPropertyProvider( )
  {
    m_properties.add( createPointProperty( IWspmConstants.POINT_PROPERTY_BREITE ) );
    m_properties.add( createPointProperty( IWspmConstants.POINT_PROPERTY_BREITE ) );
    m_properties.add( createPointProperty( IWspmConstants.POINT_PROPERTY_BEWUCHS_AX ) );
    m_properties.add( createPointProperty( IWspmConstants.POINT_PROPERTY_BEWUCHS_AY ) );
    m_properties.add( createPointProperty( IWspmConstants.POINT_PROPERTY_BEWUCHS_DP ) );
    m_properties.add( createPointProperty( IWspmConstants.POINT_PROPERTY_HOCHWERT ) );
    m_properties.add( createPointProperty( IWspmConstants.POINT_PROPERTY_RECHTSWERT ) );
    m_properties.add( createPointProperty( IWspmConstants.POINT_PROPERTY_RAUHEIT_KS ) );
    m_properties.add( createPointProperty( IWspmConstants.POINT_PROPERTY_RAUHEIT_KST ) );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilPointPropertyProvider#getPointProperties()
   */
  public IComponent[] getPointProperties( )
  {
    /**
     * the order in the array is the columnsort_order in the tableview
     */
    return this.m_properties.toArray( new IComponent[] {} );
  }

  private final IComponent createPointProperty( final String property )
  {
    // FIXME phenomen
    if( property.equals( IWspmConstants.POINT_PROPERTY_BREITE ) )
      return new Component( IWspmConstants.POINT_PROPERTY_BREITE, "Breite", "Breite", "", "", IWspmConstants.Q_DOUBLE, Double.NaN, new DictionaryPhenomenon( IWspmConstants.POINT_PROPERTY_BREITE, "Breite", "Breite" ) );
    else if( property.equals( IWspmConstants.POINT_PROPERTY_HOEHE ) )
      return new Component( IWspmConstants.POINT_PROPERTY_HOEHE, "Höhe", "Höhe", "", "", IWspmConstants.Q_DOUBLE, Double.NaN, new DictionaryPhenomenon( IWspmConstants.POINT_PROPERTY_HOEHE, "Höhe", "Höhe" ) );
    else if( property.equals( IWspmConstants.POINT_PROPERTY_BEWUCHS_AX ) )
      return new Component( IWspmConstants.POINT_PROPERTY_BEWUCHS_AX, "Bewuchs Ax", "Bewuchs Ax", "", "", IWspmConstants.Q_DOUBLE, Double.NaN, new DictionaryPhenomenon( IWspmConstants.POINT_PROPERTY_BEWUCHS_AX, "Bewuchs Ax", "Bewuchs Ax" ) );
    else if( property.equals( IWspmConstants.POINT_PROPERTY_BEWUCHS_AY ) )
      return new Component( IWspmConstants.POINT_PROPERTY_BEWUCHS_AY, "Bewuchs Ay", "Bewuchs Ay", "", "", IWspmConstants.Q_DOUBLE, Double.NaN, new DictionaryPhenomenon( IWspmConstants.POINT_PROPERTY_BEWUCHS_AY, "Bewuchs Ay", "Bewuchs Ay" ) );
    else if( property.equals( IWspmConstants.POINT_PROPERTY_BEWUCHS_DP ) )
      return new Component( IWspmConstants.POINT_PROPERTY_BEWUCHS_DP, "Bewuchs dP", "Bewuchs dP", "", "", IWspmConstants.Q_DOUBLE, Double.NaN, new DictionaryPhenomenon( IWspmConstants.POINT_PROPERTY_BEWUCHS_DP, "Bewuchs Dp", "Bewuchs Dp" ) );
    else if( property.equals( IWspmConstants.POINT_PROPERTY_RECHTSWERT ) )
      return new Component( IWspmConstants.POINT_PROPERTY_RECHTSWERT, "Rechtswert", "Rechtswert", "", "", IWspmConstants.Q_DOUBLE, Double.NaN, new DictionaryPhenomenon( IWspmConstants.POINT_PROPERTY_RECHTSWERT, "Rechtswert", "Rechtswert" ) );
    else if( property.equals( IWspmConstants.POINT_PROPERTY_HOCHWERT ) )
      return new Component( IWspmConstants.POINT_PROPERTY_HOCHWERT, "Hochwert", "Hochwert", "", "", IWspmConstants.Q_DOUBLE, Double.NaN, new DictionaryPhenomenon( IWspmConstants.POINT_PROPERTY_HOCHWERT, "Hochwert", "Hochwert" ) );
    else if( property.equals( IWspmConstants.POINT_PROPERTY_RAUHEIT_KS ) )
      return new Component( IWspmConstants.POINT_PROPERTY_RAUHEIT_KS, "Rauheit-ks", "Rauheit-ks", "", "", IWspmConstants.Q_DOUBLE, Double.NaN, new DictionaryPhenomenon( IWspmConstants.POINT_PROPERTY_RAUHEIT_KS, "Rauheit-ks", "Rauheit-ks" ) );
    else if( property.equals( IWspmConstants.POINT_PROPERTY_RAUHEIT_KST ) )
      return new Component( IWspmConstants.POINT_PROPERTY_RAUHEIT_KST, "Rauheit-kst", "Rauheit-kst", "", "", IWspmConstants.Q_DOUBLE, Double.NaN, new DictionaryPhenomenon( IWspmConstants.POINT_PROPERTY_RAUHEIT_KST, "Rauheit-kst", "Rauheit-kst" ) );

    throw new IllegalStateException( "property not defined" );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilPointPropertyProvider2#createDoubleFor(java.lang.String,
   *      java.lang.Object)
   */
  public Double createDoubleFor( final IComponent component, final Object value )
  {
    // TODO Auslesen einer Rauheiten-, oder Bewuchsklassendatei (z.B. 0.34 für Sträucher)
    return Double.NaN;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilPointPropertyProvider#providesPointProperty(org.kalypso.observation.result.IComponent)
   */
  public boolean providesPointProperty( final String property )
  {
    for( final IComponent component : m_properties )
    {
      if( component.getId().equals( property ) )
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
