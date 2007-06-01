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
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.List;

import org.kalypso.contribs.java.awt.HighlightGraphics;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.IKalypsoThemeFilter;
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
  public static double calcScale( final IMapModell model, final GM_Envelope bbox, final int mapWidth, final int mapHeight )
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
      final GM_Position min = GeometryFactory.createGM_Position( box.getMin().getX() + dx * (mapWidth / 2d - 1), box.getMin().getY() + dy * (mapHeight / 2d - 1) );
      final GM_Position max = GeometryFactory.createGM_Position( box.getMin().getX() + dx * (mapWidth / 2d), box.getMin().getY() + dy * (mapHeight / 2d) );
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

  /**
   * Creates an image of the map modell. Does wait, until all themes are loaded. That means it can take some time, until
   * this method returns. If a timeout is reached, the function will return also only a half finished image.<br>
   * ATTENTION: For GUI purposes I recommend using the other function.
   * 
   * @author Holger Albert
   */
  public static BufferedImage createCompleteImageFromModell( final GeoTransform p, final GM_Envelope bbox, final Rectangle bounds, final int width, final int height, final IMapModell model )
  {
    final BufferedImage image = new BufferedImage( width, height, BufferedImage.TYPE_INT_ARGB );
    final Graphics gr = image.getGraphics();
    try
    {
      gr.setColor( Color.white );
      gr.fillRect( 0, 0, width, height );
      gr.setColor( Color.black );
      gr.setClip( 0, 0, width, height );
      final int x = bounds.x;
      final int y = bounds.y;
      final int w = bounds.width;
      final int h = bounds.height;

      p.setDestRect( x - 2, y - 2, w + x, h + y );

      final double scale = calcScale( model, bbox, bounds.width, bounds.height );
      try
      {
        // TODO How to initialize the themes without painting?
        model.paint( gr, p, bbox, scale, false );

        final IKalypsoTheme[] allThemes = model.getAllThemes();
        int timeout = 0;
        boolean isLoading = true;
        while( isLoading )
        {
          isLoading = false;
          for( final IKalypsoTheme theme : allThemes )
          {
            if( theme.isLoaded() == false )
              isLoading = true;
          }

          /* If the timeout was reached the last run, stop waiting. */
          if( timeout >= 15000 )
            break;

          /* Wait for one second, if it is still loading. */
          if( isLoading )
          {
            Thread.sleep( 1000 );
            timeout = timeout + 1000;
          }
        }

        model.paint( gr, p, bbox, scale, false );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
        System.out.println( e.getMessage() );
      }

      final HighlightGraphics highlightGraphics = new HighlightGraphics( (Graphics2D) gr );
      model.paint( highlightGraphics, p, bbox, scale, true );
    }
    finally
    {
      gr.dispose();
    }

    return image;
  }

  /**
   * Is used to create an image of a map modell. Does not wait until all themes are loaded. Is used from the map panel
   * as well, where the drawing is done every refresh of the map. So it does not matter, when some themes finish, if
   * they finish at last.
   */
  public static BufferedImage createImageFromModell( final GeoTransform p, final GM_Envelope bbox, final Rectangle bounds, final int width, final int height, final IMapModell model )
  {
    final BufferedImage image = new BufferedImage( width, height, BufferedImage.TYPE_INT_ARGB );
    final Graphics2D gr = (Graphics2D) image.getGraphics();
    try
    {
      gr.setColor( Color.white );
      gr.fillRect( 0, 0, width, height );
      gr.setColor( Color.black );
      gr.setClip( 0, 0, width, height );

      gr.setRenderingHint( RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON );

      final int x = bounds.x;
      final int y = bounds.y;
      final int w = bounds.width;
      final int h = bounds.height;

      p.setDestRect( x - 2, y - 2, w + x, h + y );

      final double scale = calcScale( model, bbox, bounds.width, bounds.height );
      try
      {
        model.paint( gr, p, bbox, scale, false );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
        System.out.println( e.getMessage() );
      }

      final HighlightGraphics highlightGraphics = new HighlightGraphics( gr );
      model.paint( highlightGraphics, p, bbox, scale, true );
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
  private static double calcDistance( final double lon1, final double lat1, final double lon2, final double lat2 )
  {
    final double r = 6378.137;
    final double rad = Math.PI / 180d;
    double cose = 0;

    cose = Math.sin( rad * lon1 ) * Math.sin( rad * lon2 ) + Math.cos( rad * lon1 ) * Math.cos( rad * lon2 ) * Math.cos( rad * (lat1 - lat2) );
    final double dist = r * Math.acos( cose );

    return dist * 1000;
  }

  public static IKalypsoTheme[] filterThemes( final IMapModell modell, final IKalypsoThemeFilter filter )
  {
    final IKalypsoTheme[] allThemes = modell.getAllThemes();
    final List<IKalypsoTheme> themes = new ArrayList<IKalypsoTheme>( allThemes.length );
    for( final IKalypsoTheme theme : allThemes )
    {
      if( filter.accept( theme ) )
        themes.add( theme );
    }

    return themes.toArray( new IKalypsoTheme[themes.size()] );
  }

  /**
   * Calculates the common extent o fall given themes.
   * 
   * @param predicate
   *            If not <code>null</code>, only themes applying to the predicate are considered.
   * @return <code>null</code>, if the array of themes is empty or null.
   */
  public static GM_Envelope calculateExtent( final IKalypsoTheme[] themes, final IKalypsoThemePredicate predicate )
  {
    if( themes == null )
      return null;

    GM_Envelope result = null;
    for( final IKalypsoTheme kalypsoTheme : themes )
    {
      if( predicate == null || predicate.decide( kalypsoTheme ) )
      {
        final GM_Envelope boundingBox = kalypsoTheme.getBoundingBox();
        if( result == null )
          result = boundingBox;
        else
          result = result.getMerged( boundingBox );
      }
    }

    return result;
  }

}
