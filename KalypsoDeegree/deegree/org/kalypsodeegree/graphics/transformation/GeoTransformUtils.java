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
package org.kalypsodeegree.graphics.transformation;

import java.awt.Rectangle;

import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Position;

/**
 * Helper class for {@link org.kalypso.transformation.GeoTransformer}.<br>
 * Introduced in otder not to change the {@link org.kalypso.transformation.GeoTransformer} interface.<br>
 * These helper methods should probably be added to the {@link org.kalypso.transformation.GeoTransformer} once it is
 * refactored.
 *
 * @author Gernot Belger
 */
public class GeoTransformUtils
{
  private GeoTransformUtils( )
  {
    throw new UnsupportedOperationException( "Do not instantiate this helper class" );
  }

  /**
   * Utility for world2screen-transformation<br>
   * . Transforms an envelope from the destination crs to the source crs.
   */
  public static Rectangle world2screen( final GeoTransform transform, final GM_Envelope destBounds )
  {
    final GM_Position maxWorld = destBounds.getMax();
    final GM_Position minWorld = destBounds.getMin();
    final GM_Position maxScreen = transform.getDestPoint( maxWorld );
    final GM_Position minScreen = transform.getDestPoint( minWorld );

    final int x = (int) Math.min( minScreen.getX(), maxScreen.getX() );
    final int y = (int) Math.min( minScreen.getY(), maxScreen.getY() );

    final int width = (int) Math.abs( maxScreen.getX() - minScreen.getX() );
    final int height = (int) Math.abs( maxScreen.getY() - minScreen.getY() );

    return new Rectangle( x, y, width, height );
  }

}
