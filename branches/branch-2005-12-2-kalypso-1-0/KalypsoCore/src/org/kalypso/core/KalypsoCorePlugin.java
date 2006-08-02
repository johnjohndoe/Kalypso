/*--------------- Kalypso-Header ------------------------------------------

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

 --------------------------------------------------------------------------*/

package org.kalypso.core;

import java.util.TimeZone;

import org.eclipse.core.runtime.Plugin;
import org.kalypso.ogc.gml.selection.FeatureSelectionManager2;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;

/**
 * 
 * TODO: insert type comment here
 * 
 * @author belger
 */
public class KalypsoCorePlugin extends Plugin
{
  private static KalypsoCorePlugin m_default;
  private IFeatureSelectionManager m_selectionManager;

  public static KalypsoCorePlugin getDefault()
  {
    return m_default;
  }

  public KalypsoCorePlugin()
  {
    m_default = this;
  }

  public IFeatureSelectionManager getSelectionManager()
  {
    if( m_selectionManager == null )
      m_selectionManager = new FeatureSelectionManager2();

    return m_selectionManager;
  }

  /**
   * Returns the default timezone which shall be used to display date's in kalypso.
   * 
   * <p>
   * This is a bit special, we also could have used {@link TimeZone#setDefault(java.util.TimeZone)}. We do this in
   * order not to disturb other plugins. But every Kalypso Plugins should use this time zone to display and parse date
   * information.
   */
  public TimeZone getTimeZone()
  {
    // TODO: let the user edit the time-zone via user preferences
    // REMARK: if the above todo is fixed, please also support setting timezone
    // via system properties (aka config.ini file).
    // In this case, the user preferences may overwrite the global settings. 

    // get the time zone from a global place, i.e. the sstem properties
    // System properties can easily set in the eclipse config.ini file 
    final String tzString = System.getProperty( "kalypso.timezone", "UTC" );
    if( tzString != null && tzString.length() > 0 )
      return TimeZone.getTimeZone( tzString );

    return TimeZone.getDefault();
  }

}
