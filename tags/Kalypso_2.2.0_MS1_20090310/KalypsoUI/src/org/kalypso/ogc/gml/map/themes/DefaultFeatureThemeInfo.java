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
package org.kalypso.ogc.gml.map.themes;

import java.util.Formatter;
import java.util.Properties;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.CoreException;
import org.kalypso.core.KalypsoCoreExtensions;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.i18n.Messages;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.IKalypsoThemeInfo;
import org.kalypso.ui.catalogs.FeatureTypePropertiesCatalog;
import org.kalypso.ui.catalogs.IFeatureTypePropertiesConstants;
import org.kalypsodeegree.model.geometry.GM_Position;

/**
 * The default implementation if {@link IKalypsoThemeInfo}.<br>
 * This implementation will be used, if no explicit info is configured with a theme.<br>
 * This implementation tires first to determine a theme-info registered for a certain qname.<br>
 * If this fails, a default message is provided<br>
 * Else, all calls are delegated to the found theme info.
 * 
 * @author Gernot Belger
 */
public class DefaultFeatureThemeInfo implements IKalypsoThemeInfo
{
  private IKalypsoFeatureTheme m_theme;

  private IKalypsoThemeInfo m_delegate;

  /**
   * @see org.kalypso.ogc.gml.IKalypsoThemeInfo#init(org.kalypso.ogc.gml.IKalypsoTheme, java.util.Properties)
   */
  public void init( final IKalypsoTheme theme, final Properties props ) throws CoreException
  {
    Assert.isLegal( theme instanceof IKalypsoFeatureTheme );

    m_theme = (IKalypsoFeatureTheme) theme;

    /* Try to find a registered theme for the given qname */
    final IFeatureType featureType = m_theme.getFeatureType();
    if( featureType != null )
    {
      final QName qname = featureType.getQName();
      final Properties properties = FeatureTypePropertiesCatalog.getProperties( m_theme.getWorkspace().getContext(), qname );
      final String infoId = properties.getProperty( IFeatureTypePropertiesConstants.THEME_INFO_ID, null );
      if( infoId != null )
      {
        m_delegate = KalypsoCoreExtensions.createThemeInfo( infoId, theme );
        return;
      }
    }

    /* If anything else fails, create a default delegate to avoid <code>null</code>-checks. */
    m_delegate = new org.kalypso.ogc.gml.FeatureThemeInfo();
    m_delegate.init( theme, props );
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoThemeInfo#appendInfo(java.util.Formatter,
   *      org.kalypsodeegree.model.geometry.GM_Position)
   */
  public void appendInfo( final Formatter formatter, final GM_Position pos )
  {
    Assert.isNotNull( m_delegate, Messages.getString("org.kalypso.ogc.gml.map.themes.DefaultFeatureThemeInfo.0") ); //$NON-NLS-1$
    Assert.isNotNull( m_theme, Messages.getString("org.kalypso.ogc.gml.map.themes.DefaultFeatureThemeInfo.1") ); //$NON-NLS-1$

    m_delegate.appendInfo( formatter, pos );
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoThemeInfo#appendQuickInfo(java.util.Formatter,
   *      org.kalypsodeegree.model.geometry.GM_Position)
   */
  public void appendQuickInfo( final Formatter formatter, final GM_Position pos )
  {
    Assert.isNotNull( m_delegate, Messages.getString("org.kalypso.ogc.gml.map.themes.DefaultFeatureThemeInfo.2") ); //$NON-NLS-1$
    Assert.isNotNull( m_theme, Messages.getString("org.kalypso.ogc.gml.map.themes.DefaultFeatureThemeInfo.3") ); //$NON-NLS-1$

    m_delegate.appendQuickInfo( formatter, pos );
  }

}
