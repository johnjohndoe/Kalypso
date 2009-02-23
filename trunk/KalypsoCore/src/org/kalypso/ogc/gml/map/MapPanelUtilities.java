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
package org.kalypso.ogc.gml.map;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.image.BufferedImage;

import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.graphics.transformation.GeoTransformUtils;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * Helper code for {@link IMapPanel}
 *
 * @author Gernot
 */
public class MapPanelUtilities
{
  private MapPanelUtilities( )
  {
    throw new UnsupportedOperationException( "Do not instantiate helper class" );
  }

  public static GM_Envelope calcPanToLocationBoundingBox( final IMapPanel mapPanel, final double geoCenterX, final double geoCenterY )
  {
    final int width = mapPanel.getWidth();
    final int height = mapPanel.getHeight();
    final double ratio = height / width;

    final GeoTransform transform = mapPanel.getProjection();

    final double gisDX = transform.getSourceX( width / 2 ) - transform.getSourceX( 0 );
    final double gisDY = gisDX * ratio;
    final double gisX1 = geoCenterX - gisDX;
    final double gisX2 = geoCenterX + gisDX;
    final double gisY1 = geoCenterY - gisDY;
    final double gisY2 = geoCenterY + gisDY;

    return GeometryFactory.createGM_Envelope( gisX1, gisY1, gisX2, gisY2, mapPanel.getMapModell().getCoordinatesSystem() );
  }

  public static GM_Envelope calcZoomInBoundingBox( final GM_Envelope bbox, final boolean in )
  {
    final GM_Position currentMax = bbox.getMax();
    final GM_Position currentMin = bbox.getMin();

    final double factor = in == true ? 5.0 : -5.0;

    final double newMaxX = currentMax.getX() - (currentMax.getX() - currentMin.getX()) / factor;
    final double newMaxY = currentMax.getY() - (currentMax.getY() - currentMin.getY()) / factor;
    final double newMinX = currentMin.getX() + (currentMax.getX() - currentMin.getX()) / factor;
    final double newMinY = currentMin.getY() + (currentMax.getY() - currentMin.getY()) / factor;

    final GM_Position newMin = GeometryFactory.createGM_Position( newMinX, newMinY );
    final GM_Position newMax = GeometryFactory.createGM_Position( newMaxX, newMaxY );

    return GeometryFactory.createGM_Envelope( newMin, newMax, bbox.getCoordinateSystem() );
  }

  static double getRatio( final IMapPanel mapPanel )
  {
    return (double) mapPanel.getHeight() / (double) mapPanel.getWidth();
  }

  /**
   * Paints an image into the given graphics. The image represents the given extent (imageBounds).<br>
   * The given world2screen is used to fit the image to the right rectangle within the graphics.
   */
  public static void paintIntoExtent( final Graphics g, final GeoTransform g2world2screen, final BufferedImage image, final GM_Envelope imageBounds, final Color bgColor )
  {
    final Rectangle imageScreenBounds = GeoTransformUtils.world2screen( g2world2screen, imageBounds );
    final Rectangle screenBounds = new Rectangle( 0, 0, (int) g2world2screen.getDestWidth(), (int) g2world2screen.getDestHeight() );
    if( bgColor != null )
      fillExterior( g, screenBounds, imageScreenBounds, bgColor );
    g.drawImage( image, imageScreenBounds.x, imageScreenBounds.y, imageScreenBounds.width, imageScreenBounds.height, null );
  }

  /**
   * Fill the given graphics but not within the 'noFillArea'.
   */
  public static void fillExterior( final Graphics g, final Rectangle screenRect, final Rectangle noFillArea, final Color bgColor )
  {
    g.setColor( bgColor );

    final int width = screenRect.width;
    final int height = screenRect.height;
    final int left = screenRect.x;
    final int right = screenRect.x + width;
    final int top = screenRect.y;
    final int bottom = screenRect.y + height;

    // left
    if( noFillArea.x > left )
      g.fillRect( left, top, noFillArea.x - left, height );

    // right
    if( noFillArea.x < right )
      g.fillRect( noFillArea.x + noFillArea.width, top, right - noFillArea.x - noFillArea.width, height );

    // top
    if( noFillArea.y > top )
      g.fillRect( left, top, width, noFillArea.y - top );

    // bottom
    if( noFillArea.y < bottom )
      g.fillRect( left, noFillArea.y + noFillArea.height, width, bottom - noFillArea.y - noFillArea.height );
  }



}
