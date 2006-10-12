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
/*
 * Created on 22.07.2004
 * 
 * TODO To change the template for this generated file go to Window - Preferences - Java - Code Style - Code Templates
 */
package org.kalypso.ogc.gml.outline;

import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.KalypsoUserStyle;
import org.kalypsodeegree.graphics.sld.UserStyle;

public class ThemeStyleTreeObject
{
  private final KalypsoUserStyle myStyle;

  private final IKalypsoTheme myTheme;

  public ThemeStyleTreeObject( IKalypsoTheme theme, UserStyle style )
  {
    myTheme = theme;
    myStyle = (KalypsoUserStyle)style;
  }

  /**
   * @param theme
   * @param style
   */
  public ThemeStyleTreeObject( final IKalypsoFeatureTheme theme, KalypsoUserStyle style )
  {
    myTheme = theme;
    myStyle = style;
  }

  public KalypsoUserStyle getStyle()
  {
    return myStyle;
  }

  public IKalypsoTheme getTheme()
  {
    return myTheme;
  }

  @Override
  public String toString( )
  {
    if( myStyle == null )
      return "<no styles set>";

    if( myStyle.getName() != null )
      return myStyle.getName();
    return myStyle.toString();
  }
}