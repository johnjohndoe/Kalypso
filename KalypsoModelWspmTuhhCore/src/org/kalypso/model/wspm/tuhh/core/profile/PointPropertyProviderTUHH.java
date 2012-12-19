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

import org.kalypso.model.wspm.core.IWspmPointProperties;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.profil.AbstractPointPropertyProvider;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.util.ProfileUtil;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.observation.result.IComponent;

/**
 * @author kimwerner
 */
public class PointPropertyProviderTUHH extends AbstractPointPropertyProvider
{
  public PointPropertyProviderTUHH( )
  {
    m_properties.add( IWspmPointProperties.POINT_PROPERTY_BREITE );
    m_properties.add( IWspmPointProperties.POINT_PROPERTY_HOEHE );
    m_properties.add( IWspmPointProperties.POINT_PROPERTY_BEWUCHS_AX );
    m_properties.add( IWspmPointProperties.POINT_PROPERTY_BEWUCHS_AY );
    m_properties.add( IWspmPointProperties.POINT_PROPERTY_BEWUCHS_DP );
    m_properties.add( IWspmPointProperties.POINT_PROPERTY_BEWUCHS_CLASS );

    m_properties.add( IWspmPointProperties.POINT_PROPERTY_HOCHWERT );
    m_properties.add( IWspmPointProperties.POINT_PROPERTY_RECHTSWERT );
    m_properties.add( IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KS );
    m_properties.add( IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KST );
    m_properties.add( IWspmPointProperties.POINT_PROPERTY_ROUGHNESS_CLASS );

    m_properties.add( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE );
    m_properties.add( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR );
    m_properties.add( IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE );

    m_properties.add( IWspmPointProperties.POINT_PROPERTY_COMMENT );
    m_properties.add( IWspmPointProperties.POINT_PROPERTY_CODE );

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

  @Override
  public IProfile createProfil( final IProfileFeature source )
  {
    final TuhhProfil profil = new TuhhProfil( source );

    // Create required properties
    profil.addPointProperty( getPointProperty( IWspmPointProperties.POINT_PROPERTY_BREITE ) );
    profil.addPointProperty( getPointProperty( IWspmPointProperties.POINT_PROPERTY_HOEHE ) );
    profil.addPointProperty( getPointProperty( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE ) );
    profil.addPointProperty( getPointProperty( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ) );

    return profil;
  }

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
    return ProfileUtil.getFeatureComponent( propertyId );
  }
}
