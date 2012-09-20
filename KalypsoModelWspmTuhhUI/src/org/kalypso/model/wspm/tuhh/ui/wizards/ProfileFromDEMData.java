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
package org.kalypso.model.wspm.tuhh.ui.wizards;

import org.apache.commons.lang3.StringUtils;
import org.kalypso.commons.java.util.AbstractModelObject;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;

/**
 * @author Gernot Belger
 */
public class ProfileFromDEMData extends AbstractModelObject
{
  static final String PROPERTY_NAME = "name"; //$NON-NLS-1$

  static final String PROPERTY_STATION = "station"; //$NON-NLS-1$

  public static final String PROPERTY_THEME = "theme"; //$NON-NLS-1$

  private final IProfile m_profile;

  private final IKalypsoFeatureTheme[] m_profileThemes;

  private IKalypsoFeatureTheme m_theme;

  public ProfileFromDEMData( final IProfile profile, final IKalypsoFeatureTheme[] profileThemes )
  {
    m_profile = profile;

    profile.setName( StringUtils.EMPTY );

    m_profileThemes = profileThemes;
    m_theme = m_profileThemes[0];
  }

  public IKalypsoFeatureTheme getTheme( )
  {
    return m_theme;
  }

  public void setTheme( final IKalypsoFeatureTheme theme )
  {
    final IKalypsoFeatureTheme oldValue = m_theme;

    m_theme = theme;

    firePropertyChange( PROPERTY_THEME, oldValue, theme );
  }

  public IProfile getProfile( )
  {
    return m_profile;
  }

  public String getName( )
  {
    return m_profile.getName();
  }

  public void setName( final String name )
  {
    final String oldValue = getName();

    m_profile.setName( name );

    firePropertyChange( PROPERTY_NAME, oldValue, name );
  }

  public Double getStation( )
  {
    final double station = m_profile.getStation();
    if( Double.isNaN( station ) )
      return null;

    return station;
  }

  public void setStation( final Double station )
  {
    final Double oldValue = station;

    if( Double.isNaN( station ) )
      m_profile.setStation( Double.NaN );
    else
      m_profile.setStation( station );

    firePropertyChange( PROPERTY_STATION, oldValue, station );
  }

  public IKalypsoFeatureTheme[] getThemes( )
  {
    return m_profileThemes;
  }
}