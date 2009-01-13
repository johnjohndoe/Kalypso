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
package org.kalypso.model.wspm.ui.view.table;

import org.eclipse.core.resources.IMarker;

/**
 * @author Gernot Belger
 */
public class MarkerUtils
{
  private MarkerUtils( )
  {
    throw new UnsupportedOperationException( org.kalypso.model.wspm.ui.i18n.Messages.getString("org.kalypso.model.wspm.ui.view.table.MarkerUtils.0") ); //$NON-NLS-1$
  }

  /**
   * Returns the specified attribute of the given marker as an integer. Returns the given default if the attribute value
   * is not an integer.
   * 
   * @param marker
   *            the marker
   * @param attributeName
   *            the name of the attribute
   * @param defaultValue
   *            the default value
   * @return the attribute's value or the default value if the attribute does not exist or isn't an int
   */
  private static int getIntAttribute( final IMarker marker, final String attributeName, final int defaultValue )
  {
    if( marker.exists() )
      return marker.getAttribute( attributeName, defaultValue );
    return defaultValue;
  }

  /**
   * Returns the severity of the given marker.
   * 
   * @param marker
   *            the marker
   * @return the priority, or <code>IMarker.SEVERITY_INFO</code> if not set
   * @see IMarker#SEVERITY
   * @see IMarker#SEVERITY_INFO
   * @see IMarker#getAttribute(java.lang.String,int)
   */
  public static int getSeverity( final IMarker marker )
  {
    return getIntAttribute( marker, IMarker.SEVERITY, IMarker.SEVERITY_INFO );
  }

  /**
   * Returns the marker with the severest severity.
   */
  public static IMarker worstOf( final IMarker[] recordMarkers )
  {
    IMarker worst = null;
    for( final IMarker marker : recordMarkers )
    {
      // if it is an error, immediately return
      final int severity = getSeverity( marker );
      if( severity == IMarker.SEVERITY_ERROR )
        return marker;

      final int worstSeverity = worst == null ? IMarker.SEVERITY_INFO : getSeverity( worst );
      if( severity == IMarker.SEVERITY_WARNING && worstSeverity == IMarker.SEVERITY_INFO )
        worst = marker;
      else if( worst == null )
        worst = marker;
    }

    return worst;
  }

}
