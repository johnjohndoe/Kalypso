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
package org.kalypso.kalypsomodel1d2d.ui.map.channeledit;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.model.wspm.core.gml.ProfileFeatureFactory;
import org.kalypso.model.wspm.core.gml.WspmProfile;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilEventManager;
import org.kalypso.model.wspm.core.profil.ProfilDataException;
import org.kalypso.model.wspm.core.profil.impl.ProfilEventManager;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * State object for create main channel widget and composite.
 * 
 * @author Thomas Jung
 */
public class CreateChannelData
{
  private IKalypsoFeatureTheme m_profileTheme;

  private IKalypsoFeatureTheme m_bankTheme;

  private Set<Feature> m_selectedProfiles = new HashSet<Feature>();

  private final CreateMainChannelWidget m_widget;

  public CreateChannelData( final CreateMainChannelWidget widget )
  {
    m_widget = widget;
  }

  public IKalypsoFeatureTheme getProfileTheme( )
  {
    return m_profileTheme;
  }

  public IKalypsoFeatureTheme getBankTheme( )
  {
    return m_bankTheme;
  }

  public void setProfileTheme( final IKalypsoFeatureTheme profileTheme )
  {
    m_profileTheme = profileTheme;

    // TODO: event handling
  }

  public void setBankTheme( final IKalypsoFeatureTheme bankTheme )
  {
    m_bankTheme = bankTheme;

    // TODO: event handling
  }

  /**
   * Gets the WSPM profile themes in the Kalypso theme list
   */
  public IKalypsoFeatureTheme[] getProfileThemes( )
  {
    final IMapModell mapModell = m_widget.getPanel().getMapModell();
    if( mapModell == null )
      return new IKalypsoFeatureTheme[0];
    
    final IKalypsoTheme[] allThemes = mapModell.getAllThemes();

    final List<IKalypsoFeatureTheme> goodThemes = new ArrayList<IKalypsoFeatureTheme>();

    for( final IKalypsoTheme theme : allThemes )
    {
      if( theme instanceof IKalypsoFeatureTheme )
      {
        final IKalypsoFeatureTheme fTheme = (IKalypsoFeatureTheme) theme;
        final IFeatureType featureType = fTheme.getFeatureType();

        if( featureType != null && GMLSchemaUtilities.substitutes( featureType, WspmProfile.QNAME_PROFILE ) )
          goodThemes.add( fTheme );
      }
    }

    return goodThemes.toArray( new IKalypsoFeatureTheme[goodThemes.size()] );
  }

  /**
   * Gets the linestring themes in the Kalypso theme list
   */
  public IKalypsoFeatureTheme[] getBankThemes( )
  {
    final IKalypsoTheme[] allThemes = m_widget.getPanel().getMapModell().getAllThemes();

    final List<IKalypsoFeatureTheme> goodThemes = new ArrayList<IKalypsoFeatureTheme>();

    for( final IKalypsoTheme theme : allThemes )
    {
      if( theme instanceof IKalypsoFeatureTheme )
      {
        final IKalypsoFeatureTheme fTheme = (IKalypsoFeatureTheme) theme;
        final IFeatureType featureType = fTheme.getFeatureType();

        if( featureType == null )
          return new IKalypsoFeatureTheme[0];
        final IValuePropertyType[] allGeomteryProperties = featureType.getAllGeomteryProperties();
        // choose only the linestrings
        // just take the first found property
        if( allGeomteryProperties.length > 0 && allGeomteryProperties[0].getValueQName().equals( GeometryUtilities.QN_LINE_STRING_PROPERTY ) )
          goodThemes.add( fTheme );
      }
    }
    return goodThemes.toArray( new IKalypsoFeatureTheme[goodThemes.size()] );
  }

  public void addSelectedProfiles( final Feature[] profileFeatures )
  {
    m_selectedProfiles.addAll( Arrays.asList( profileFeatures ) );

    m_widget.update();
  }

  public void removeSelectedProfiles( final Feature[] profileFeatures )
  {
    m_selectedProfiles.removeAll( Arrays.asList( profileFeatures ) );

    m_widget.update();
  }

  public Feature[] getSelectedProfiles( )
  {
    return m_selectedProfiles.toArray( new Feature[m_selectedProfiles.size()] );
  }

  public IProfilEventManager getProfilEventManager( )
  {
    if( m_selectedProfiles.size() == 0 )
      return new ProfilEventManager( null, null );

    try
    {
      final Feature profileFeature = m_selectedProfiles.iterator().next();
      final IProfil profil = ProfileFeatureFactory.toProfile( profileFeature );
      final ProfilEventManager pem = new ProfilEventManager( profil, null );
      return pem;
    }
    catch( final ProfilDataException e )
    {
      e.printStackTrace();
    }
    return new ProfilEventManager( null, null );
  }

}
