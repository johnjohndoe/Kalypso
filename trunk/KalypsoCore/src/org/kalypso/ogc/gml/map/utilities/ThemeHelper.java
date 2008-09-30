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
package org.kalypso.ogc.gml.map.utilities;

import java.util.LinkedHashSet;
import java.util.Set;

import org.apache.commons.lang.ArrayUtils;
import org.kalypso.ogc.gml.IKalypsoTheme;

/**
 * @author Dirk Kuch
 */
public class ThemeHelper
{

  /**
   * @param propertyID
   *            {@link org.kalypso.template.types.StyledLayerType.Property}.name
   * @param propertyValue
   *            {@link org.kalypso.template.types.StyledLayerType.Property}.value
   */
  public static IKalypsoTheme[] getThemeByProperty( final IKalypsoTheme[] themes, final String propertyID, final String propertyValue )
  {
    return getThemeByProperties( themes, propertyID, new String[] { propertyValue } );
  }

  public static IKalypsoTheme[] getThemeByProperties( IKalypsoTheme[] themes, String themeId, String[] validThemes )
  {
    final Set<IKalypsoTheme> myThemes = new LinkedHashSet<IKalypsoTheme>();

    for( final IKalypsoTheme theme : themes )
    {
      final String property = theme.getProperty( themeId, "" );
      if( ArrayUtils.contains( validThemes, property ) )
        myThemes.add( theme );
    }

    return myThemes.toArray( new IKalypsoTheme[] {} );
  }

}
