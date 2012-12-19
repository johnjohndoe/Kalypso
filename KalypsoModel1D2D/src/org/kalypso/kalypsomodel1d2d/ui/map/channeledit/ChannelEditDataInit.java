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

import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.mapmodel.IKalypsoThemePredicate;
import org.kalypso.ogc.gml.mapmodel.IKalypsoThemeVisitor;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.mapmodel.KalypsoFeatureThemeHelper;
import org.kalypso.ogc.gml.mapmodel.visitor.KalypsoThemeVisitor;

/**
 * Helper that (re-)initializes the channel data.
 *
 * @author Gernot Belger
 */
class ChannelEditDataInit
{
  private final ChannelEditData m_data;

  private final IMapPanel m_panel;

  public ChannelEditDataInit( final ChannelEditData data, final IMapPanel panel )
  {
    m_data = data;
    m_panel = panel;
  }

  void init( )
  {
    initProfileThemes();
    initBankThemes();

    // TODO: other data init as well: discretisation model
  }

  private void initProfileThemes( )
  {
    final IKalypsoFeatureTheme[] profileThemes = findProfileThemes();
    m_data.setProfileThemes( profileThemes );
  }

  private void initBankThemes( )
  {
    if( m_panel == null )
      m_data.setBankThemes( new IKalypsoFeatureTheme[0] );
    else
    {
      final IKalypsoFeatureTheme[] bankThemes = KalypsoFeatureThemeHelper.getLineThemes( m_panel );
      m_data.setBankThemes( bankThemes );
    }
  }

  /**
   * Gets the WSPM profile themes in the Kalypso theme list
   */
  private IKalypsoFeatureTheme[] findProfileThemes( )
  {
    if( m_panel == null )
      return new IKalypsoFeatureTheme[0];

    final IMapModell mapModell = m_panel.getMapModell();
    if( mapModell == null )
      return new IKalypsoFeatureTheme[0];

    final IKalypsoThemePredicate profileThemePredicate = new IKalypsoThemePredicate()
    {
      @Override
      public boolean decide( final IKalypsoTheme theme )
      {
        if( theme instanceof IKalypsoFeatureTheme )
        {
          final IKalypsoFeatureTheme fTheme = (IKalypsoFeatureTheme) theme;
          final IFeatureType featureType = fTheme.getFeatureType();

          if( featureType != null && GMLSchemaUtilities.substitutes( featureType, IProfileFeature.FEATURE_PROFILE ) )
            return true;
        }

        return false;
      }
    };
    final KalypsoThemeVisitor visitor = new KalypsoThemeVisitor( profileThemePredicate );

    mapModell.accept( visitor, IKalypsoThemeVisitor.DEPTH_INFINITE );

    final IKalypsoTheme[] foundThemes = visitor.getFoundThemes();
    final IKalypsoFeatureTheme[] goodThemes = new IKalypsoFeatureTheme[foundThemes.length];
    System.arraycopy( foundThemes, 0, goodThemes, 0, foundThemes.length );

    return goodThemes;
  }
}
