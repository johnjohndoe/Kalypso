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
package org.kalypso.model.wspm.tuhh.core.profile;

import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.AbstractPointPropertyProvider;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.TupleResult;

/**
 * @author kimwerner
 */
public class PointPropertyProviderTUHH extends AbstractPointPropertyProvider
{
  public PointPropertyProviderTUHH( )
  {
    m_properties.add( IWspmConstants.POINT_PROPERTY_BREITE );
    m_properties.add( IWspmConstants.POINT_PROPERTY_HOEHE );
    m_properties.add( IWspmConstants.POINT_PROPERTY_BEWUCHS_AX );
    m_properties.add( IWspmConstants.POINT_PROPERTY_BEWUCHS_AY );
    m_properties.add( IWspmConstants.POINT_PROPERTY_BEWUCHS_DP );
    m_properties.add( IWspmConstants.POINT_PROPERTY_HOCHWERT );
    m_properties.add( IWspmConstants.POINT_PROPERTY_RECHTSWERT );
    m_properties.add( IWspmConstants.POINT_PROPERTY_RAUHEIT_KS );
    m_properties.add( IWspmConstants.POINT_PROPERTY_RAUHEIT_KST );

    m_properties.add( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE );
    m_properties.add( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR );
    m_properties.add( IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE );

    m_properties.add( IWspmTuhhConstants.POINT_PROPERTY_COMMENT );

    // Markers
    /**
     * see #isMarker(IComponent)
     */
    m_markers.add( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
    m_markers.add( IWspmTuhhConstants.MARKER_TYP_BORDVOLL );
    m_markers.add( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE );
    m_markers.add( IWspmTuhhConstants.MARKER_TYP_WEHR );

    // Markers are properties also
    m_properties.addAll( m_markers );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilPointPropertyProvider#createProfil(org.kalypso.observation.result.TupleResult)
   */
  @Override
  public IProfil createProfil( final TupleResult result )
  {
    if( result.getComponents().length == 0 )
    {
      // Special case: result is yet empty: this can happen for a new profile created from a new profile-feature
      result.addComponent( getPointProperty( IWspmConstants.POINT_PROPERTY_BREITE ) );
      result.addComponent( getPointProperty( IWspmConstants.POINT_PROPERTY_HOEHE ) );
      result.addComponent( getPointProperty( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE ) );
      result.addComponent( getPointProperty( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ) );
    }

    return new TuhhProfil( result );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilPointPropertyProvider#getDefaultValue(java.lang.String)
   */
  @Override
  public Object getDefaultValue( final String propertyID )
  {
    // HACK: @see FeatureComponent#getDefaultValue()
    if( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE.equals( propertyID ) )
      return Boolean.TRUE;

    if( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE.equals( propertyID ) )
      return "low"; //$NON-NLS-1$

    if( IWspmTuhhConstants.MARKER_TYP_BORDVOLL.equals( propertyID ) )
      return Boolean.TRUE;

    if( IWspmTuhhConstants.MARKER_TYP_WEHR.equals( propertyID ) )
      return new Double( 0.0 );

    return super.getDefaultValue( propertyID );

  }

  @Override
  public IComponent getPointProperty( final String propertyId )
  {
    return ProfilUtil.getFeatureComponent( propertyId );
  }

}
