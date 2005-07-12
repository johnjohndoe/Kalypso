/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ui.preferences;

import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.kalypso.ui.KalypsoGisPlugin;

/**
 * TODO: Developper: set your default preferences here.
 * 
 * @author schlienger
 */
public class KalypsoPreferenceInitializer extends AbstractPreferenceInitializer
{
  /**
   * @see org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer#initializeDefaultPreferences()
   */
  public void initializeDefaultPreferences()
  {
    // location of the server configuration for the clients
    KalypsoGisPlugin.getDefault().getPluginPreferences().setDefault( IKalypsoPreferences.CLIENT_CONF_URLS,
        "http://SERVER_NAME:8080/webdav/kalypso-client.ini" );

    // size of the image export for the observation chart
    KalypsoGisPlugin.getDefault().getPluginPreferences().setDefault( IKalypsoPreferences.CHART_EXPORT_WIDTH, 600 );
    KalypsoGisPlugin.getDefault().getPluginPreferences().setDefault( IKalypsoPreferences.CHART_EXPORT_HEIGHT, 500 );

    KalypsoGisPlugin.getDefault().getPluginPreferences().setDefault( IKalypsoPreferences.GLOBAL_CRS, "EPSG:31469" );

    KalypsoGisPlugin.getDefault().getPluginPreferences().setDefault( IKalypsoPreferences.LANGUAGE, "de" );
  }
}
