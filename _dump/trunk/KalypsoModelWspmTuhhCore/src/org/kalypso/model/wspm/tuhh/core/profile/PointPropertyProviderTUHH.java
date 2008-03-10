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
package org.kalypso.model.wspm.tuhh.core.profile;

import java.util.LinkedHashSet;
import java.util.Set;

import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointPropertyProvider;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.observation.phenomenon.DictionaryPhenomenon;
import org.kalypso.observation.result.Component;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.TupleResult;

/**
 * @author kimwerner
 */
public class PointPropertyProviderTUHH implements IProfilPointPropertyProvider
{
  private final Set<IComponent> m_properties = new LinkedHashSet<IComponent>();

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilPointPropertyProvider#createProfil()
   */
  public IProfil createProfil( )
  {
    return createProfil( new TupleResult() );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilPointPropertyProvider#createProfil(org.kalypso.observation.result.TupleResult)
   */
  public IProfil createProfil( final TupleResult result )
  {
    if( result.getComponents().length == 0 )
    {
      // Special case: result is yet empty: this can happen for a new profile are for a profile created from a new
      // profile-feature
      result.addComponent( getPointProperty( IWspmConstants.POINT_PROPERTY_BREITE ) );
      result.addComponent( getPointProperty( IWspmConstants.POINT_PROPERTY_HOEHE ) );
      result.addComponent( getPointProperty( IWspmConstants.POINT_PROPERTY_RAUHEIT_KS ) );
      result.addComponent( getPointProperty( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE ) );
      result.addComponent( getPointProperty( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ) );
    }

    return new TuhhProfil( result );
  }

  public PointPropertyProviderTUHH( )
  {
    m_properties.add( createPointProperty( IWspmConstants.POINT_PROPERTY_BREITE ) );
    m_properties.add( createPointProperty( IWspmConstants.POINT_PROPERTY_HOEHE ) );
    m_properties.add( createPointProperty( IWspmConstants.POINT_PROPERTY_BEWUCHS_AX ) );
    m_properties.add( createPointProperty( IWspmConstants.POINT_PROPERTY_BEWUCHS_AY ) );
    m_properties.add( createPointProperty( IWspmConstants.POINT_PROPERTY_BEWUCHS_DP ) );
    m_properties.add( createPointProperty( IWspmConstants.POINT_PROPERTY_HOCHWERT ) );
    m_properties.add( createPointProperty( IWspmConstants.POINT_PROPERTY_RECHTSWERT ) );
    m_properties.add( createPointProperty( IWspmConstants.POINT_PROPERTY_RAUHEIT_KS ) );
    m_properties.add( createPointProperty( IWspmConstants.POINT_PROPERTY_RAUHEIT_KST ) );

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
    // FIXME phenomen
    if( property.equals( IWspmConstants.POINT_PROPERTY_BREITE ) )
      return new Component( IWspmConstants.POINT_PROPERTY_BREITE, "Breite", "Breite", "", "", IWspmConstants.Q_DOUBLE, 0.0, new DictionaryPhenomenon( IWspmConstants.POINT_PROPERTY_BREITE, "Breite", "Breite" ) );

    if( property.equals( IWspmConstants.POINT_PROPERTY_HOEHE ) )
      return new Component( IWspmConstants.POINT_PROPERTY_HOEHE, "Höhe", "Höhe", "", "", IWspmConstants.Q_DOUBLE, 0.0, new DictionaryPhenomenon( IWspmConstants.POINT_PROPERTY_HOEHE, "Höhe", "Höhe" ) );

    if( property.equals( IWspmConstants.POINT_PROPERTY_BEWUCHS_AX ) )
      return new Component( IWspmConstants.POINT_PROPERTY_BEWUCHS_AX, "Bewuchs Ax", "Bewuchs Ax", "", "", IWspmConstants.Q_DOUBLE, 0.0, new DictionaryPhenomenon( IWspmConstants.POINT_PROPERTY_BEWUCHS_AX, "Bewuchs Ax", "Bewuchs Ax" ) );

    if( property.equals( IWspmConstants.POINT_PROPERTY_BEWUCHS_AY ) )
      return new Component( IWspmConstants.POINT_PROPERTY_BEWUCHS_AY, "Bewuchs Ay", "Bewuchs Ay", "", "", IWspmConstants.Q_DOUBLE, 0.0, new DictionaryPhenomenon( IWspmConstants.POINT_PROPERTY_BEWUCHS_AY, "Bewuchs Ay", "Bewuchs Ay" ) );

    if( property.equals( IWspmConstants.POINT_PROPERTY_BEWUCHS_DP ) )
      return new Component( IWspmConstants.POINT_PROPERTY_BEWUCHS_DP, "Bewuchs dP", "Bewuchs dP", "", "", IWspmConstants.Q_DOUBLE, 0.0, new DictionaryPhenomenon( IWspmConstants.POINT_PROPERTY_BEWUCHS_DP, "Bewuchs Dp", "Bewuchs Dp" ) );

    if( property.equals( IWspmConstants.POINT_PROPERTY_RECHTSWERT ) )
      return new Component( IWspmConstants.POINT_PROPERTY_RECHTSWERT, "Rechtswert", "Rechtswert", "", "", IWspmConstants.Q_DOUBLE, Double.NaN, new DictionaryPhenomenon( IWspmConstants.POINT_PROPERTY_RECHTSWERT, "Rechtswert", "Rechtswert" ) );

    if( property.equals( IWspmConstants.POINT_PROPERTY_HOCHWERT ) )
      return new Component( IWspmConstants.POINT_PROPERTY_HOCHWERT, "Hochwert", "Hochwert", "", "", IWspmConstants.Q_DOUBLE, Double.NaN, new DictionaryPhenomenon( IWspmConstants.POINT_PROPERTY_HOCHWERT, "Hochwert", "Hochwert" ) );

    if( property.equals( IWspmConstants.POINT_PROPERTY_RAUHEIT_KS ) )
      return new Component( IWspmConstants.POINT_PROPERTY_RAUHEIT_KS, "Rauheit-ks", "Rauheit-ks", "", "", IWspmConstants.Q_DOUBLE, 0.0, new DictionaryPhenomenon( IWspmConstants.POINT_PROPERTY_RAUHEIT_KS, "Rauheit-ks", "Rauheit-ks" ) );

    if( property.equals( IWspmConstants.POINT_PROPERTY_RAUHEIT_KST ) )
      return new Component( IWspmConstants.POINT_PROPERTY_RAUHEIT_KST, "Rauheit-kst", "Rauheit-kst", "", "", IWspmConstants.Q_DOUBLE, 0.0, new DictionaryPhenomenon( IWspmConstants.POINT_PROPERTY_RAUHEIT_KST, "Rauheit-kst", "Rauheit-kst" ) );

    if( property.equals( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE ) )
      return new Component( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE, "Oberkante Brücke", "Oberkante Brücke", "", "", IWspmConstants.Q_DOUBLE, Double.NaN, new DictionaryPhenomenon( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE, "Oberkante Brücke", "Oberkante Brücke" ) );

    if( property.equals( IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE ) )
      return new Component( IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE, "Unterkante Brücke", "Unterkante Brücke", "", "", IWspmConstants.Q_DOUBLE, Double.NaN, new DictionaryPhenomenon( IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE, "Unterkante Brücke", "Unterkante Brücke" ) );

    if( property.equals( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR ) )
      return new Component( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR, "Oberkante Wehr", "Oberkante Wehr", "", "", IWspmConstants.Q_DOUBLE, Double.NaN, new DictionaryPhenomenon( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR, "Oberkante Wehr", "Oberkante Wehr" ) );

    if( property.equals( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ) )
      return new Component( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE, "Trennflaeche", "Trennflaeche", "", "", IWspmConstants.Q_STRING, "none", new DictionaryPhenomenon( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE, "Trennfläche", "Trennfläche" ) );

    if( property.equals( IWspmTuhhConstants.MARKER_TYP_BORDVOLL ) )
      return new Component( IWspmTuhhConstants.MARKER_TYP_BORDVOLL, "Bordvoll", "Bordvoll", "", "", IWspmConstants.Q_BOOLEAN, Boolean.FALSE, new DictionaryPhenomenon( IWspmTuhhConstants.MARKER_TYP_BORDVOLL, "Bordvoll", "Bordvoll" ) );

    if( property.equals( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE ) )
      return new Component( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE, "Durchströmter Bereich", "Durchströmter Bereich", "", "", IWspmConstants.Q_BOOLEAN, Boolean.FALSE, new DictionaryPhenomenon( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE, "Durchströmter Bereich", "Duchströmter Bereich" ) );

    if( property.equals( IWspmTuhhConstants.MARKER_TYP_WEHR ) )
      return new Component( IWspmTuhhConstants.MARKER_TYP_WEHR, "Wehrfeldtrenner", "Wehrfeldtrenner", "", "", IWspmConstants.Q_DOUBLE, Double.NaN, new DictionaryPhenomenon( IWspmTuhhConstants.MARKER_TYP_WEHR, "Wehrfeldtrenner", "Wehrfeldtrenner" ) );

    throw new IllegalStateException( "property not defined" );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilPointPropertyProvider#providesPointProperty(java.lang.String)
   */
  // TODO: bad performance!
  public boolean providesPointProperty( final String profilPointProperty )
  {
    for( final IComponent component : m_properties )
    {
      if( component.getId().equals( profilPointProperty ) )
        return true;
    }

    return false;
  }

  // TODO: bad performance!
  public IComponent getPointProperty( final String propertyId )
  {
    for( final IComponent component : m_properties )
    {
      if( component.getId().equals( propertyId ) )
        return component;
    }

    return null;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilPointPropertyProvider#checkComponents(org.kalypso.observation.result.TupleResult)
   */
  public void checkComponents( final TupleResult result ) throws IllegalArgumentException
  {
    // TODO do it
  }

}
