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

import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Envelope;
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

  public static GM_Envelope calcPanToPixelBoundingBox( final IMapPanel mapPanel, final double screenCenterX, final double screenCenterY )
  {
    final GeoTransform transform = mapPanel.getProjection();

    final double gisMX = transform.getSourceX( screenCenterX );
    final double gisMY = transform.getSourceY( screenCenterY );
    return calcPanToLocationBoundingBox( mapPanel, gisMX, gisMY );
  }

  public static GM_Envelope calcZoomOutBoundingBox( final IMapPanel mapPanel )
  {
    final GeoTransform transform = mapPanel.getProjection();
    final double ratio = getRatio( mapPanel );
    final double gisMX = transform.getSourceX( mapPanel.getWidth() / 2d );
    final double gisMY = transform.getSourceY( mapPanel.getHeight() / 2d );

    final double gisDX = 2 * (gisMX - transform.getSourceX( 0 ));
    final double gisDY = gisDX * ratio;
    final double gisX1 = gisMX - gisDX;
    final double gisX2 = gisMX + gisDX;
    final double gisY1 = gisMY - gisDY;
    final double gisY2 = gisMY + gisDY;

    return GeometryFactory.createGM_Envelope( gisX1, gisY1, gisX2, gisY2, mapPanel.getMapModell().getCoordinatesSystem() );
  }

  static double getRatio( final IMapPanel mapPanel )
  {
    return (double) mapPanel.getHeight() / (double) mapPanel.getWidth();
  }



}
