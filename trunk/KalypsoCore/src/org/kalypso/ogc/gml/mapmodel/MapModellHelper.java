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
package org.kalypso.ogc.gml.mapmodel;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.image.BufferedImage;

import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.ct.GeoTransformer;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.tools.Debug;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * Helper class with static members
 * 
 * @author Belger
 */
public class MapModellHelper
{
  /**
   * calculates the map scale (denominator) as defined in the OGC SLD 1.0.0 specification
   * 
   * @return scale of the map
   */
  public static double calcScale( final IMapModell model, final GM_Envelope bbox, final int mapWidth,
      final int mapHeight )
  {
    try
    {
      final CS_CoordinateSystem crs = model.getCoordinatesSystem();

      if( bbox == null )
        return 0.0;

      final GM_Envelope box;
      if( !crs.getName().equalsIgnoreCase( "EPSG:4326" ) )
      {
        // transform the bounding box of the request to EPSG:4326
        final GeoTransformer transformer = new GeoTransformer( "EPSG:4326" );
        box = transformer.transformEnvelope( bbox, crs );
      }
      else
        box = bbox;

      if( box == null )
        return 0.0;

      final double dx = box.getWidth() / mapWidth;
      final double dy = box.getHeight() / mapHeight;

      // create a box on the central map pixel to determine its size in meters
      final GM_Position min = GeometryFactory.createGM_Position( box.getMin().getX() + dx * ( mapWidth / 2d - 1 ), box
          .getMin().getY()
          + dy * ( mapHeight / 2d - 1 ) );
      final GM_Position max = GeometryFactory.createGM_Position( box.getMin().getX() + dx * ( mapWidth / 2d ), box
          .getMin().getY()
          + dy * ( mapHeight / 2d ) );
      final double distance = calcDistance( min.getY(), min.getX(), max.getY(), max.getX() );

      // default pixel size defined in SLD specs is 28mm
      final double scale = distance / 0.00028;
      return scale;
    }
    catch( final Exception e )
    {
      Debug.debugException( e, "Exception occured when calculating scale!" );
    }

    return 0.0;
  }

  public static BufferedImage createImageFromModell( final GeoTransform p, final GM_Envelope bbox,
      final Rectangle bounds, final int width, final int height, final IMapModell model )
  {
    final BufferedImage image = new BufferedImage( width, height, BufferedImage.TYPE_INT_ARGB );
    final Graphics gr = image.getGraphics();
    try
    {
      gr.setColor( Color.white );
      gr.fillRect( 0, 0, width, height );
      gr.setColor( Color.black );
      gr.setClip( 0, 0, width, height );
      int x = bounds.x;
      int y = bounds.y;
      int w = bounds.width;
      int h = bounds.height;

      p.setDestRect( x - 2, y - 2, w + x, h + y );

      final double scale = calcScale( model, bbox, bounds.width, bounds.height );
      model.paintUnselected( gr, p, bbox, scale );
      gr.setXORMode( Color.yellow );
      model.paintSelected( gr, p, bbox, scale );
      //      gr.setPaintMode();

      //            Graphics hg = new HighlightGraphics( (Graphics2D)gr,
      //       Color.YELLOW.getRed() / 2, Color.YELLOW.getGreen() / 2,
      //       Color.YELLOW.getBlue() / 2,
      //          Colo//r.YELLOW.getAlpha() / 2 );
    }
    finally
    {
      gr.dispose();
    }

    return image;
  }

  /**
   * calculates the distance in meters between two points in EPSG:4326 coodinates .
   */
  private static double calcDistance( double lon1, double lat1, double lon2, double lat2 )
  {
    double r = 6378.137;
    double rad = Math.PI / 180d;
    double cose = 0;

    cose = Math.sin( rad * lon1 ) * Math.sin( rad * lon2 ) + Math.cos( rad * lon1 ) * Math.cos( rad * lon2 )
        * Math.cos( rad * ( lat1 - lat2 ) );
    double dist = r * Math.acos( cose );

    return dist * 1000;
  }

}
