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
package org.kalypsodeegree_impl.graphics.displayelements;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Polygon;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.TexturePaint;
import java.awt.geom.Area;
import java.awt.image.BufferedImage;

import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.graphics.sld.Fill;
import org.kalypsodeegree.graphics.sld.GraphicFill;
import org.kalypsodeegree.graphics.sld.Stroke;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;
import org.kalypsodeegree_impl.graphics.sld.Symbolizer_Impl.UOM;
import org.kalypsodeegree_impl.tools.Debug;

/**
 * @author Thomas Jung
 */
public class FillPolygonPainter
{
  private double m_width;

  private final Color m_fillColor;

  private final Color m_strokeColor;

  private final GraphicFill m_gFill;

  private final BufferedImage m_texture;

  private final float[] m_dash;

  private final int m_cap;

  private final int m_join;

  private final float m_dashOffset;

  private final GeoTransform m_projection;

  public FillPolygonPainter( final Fill fill, final Stroke stroke, final Feature feature, final UOM uom, final GeoTransform projection ) throws FilterEvaluationException
  {
    m_projection = projection;

    if( stroke != null )
    {
      m_width = stroke.getWidth( feature );
      m_strokeColor = makeColorWithAlpha( stroke.getStroke( feature ), stroke.getOpacity( feature ) );
      m_dash = stroke.getDashArray( feature );
      m_cap = stroke.getLineCap( feature );
      m_join = stroke.getLineJoin( feature );
      m_dashOffset = stroke.getDashOffset( feature );
    }
    else
    {
      m_strokeColor = null;
      m_width = 1;
      m_dash = null;
      m_cap = 0;
      m_join = 0;
      m_dashOffset = 0;
    }

    m_fillColor = makeColorWithAlpha( fill.getFill( feature ), fill.getOpacity( feature ) );

    m_gFill = fill.getGraphicFill();

    if( m_gFill != null )
      m_texture = m_gFill.getGraphic().getAsImage( feature, uom, projection );
    else
      m_texture = null;
  }

  public static Color makeColorWithAlpha( final Color color, final double opacity )
  {
    final int alpha = (int) Math.round( opacity * 255 );
    final int red = color.getRed();
    final int green = color.getGreen();
    final int blue = color.getBlue();
    return new Color( red, green, blue, alpha );
  }

  // TODO: make static and move to helper class
  // give non-static stuff from outside
  private <T extends GM_SurfacePatch> Area calcTargetCoordinates( final GM_Position[] outerRing, final GM_Position[][] innerRings ) throws Exception
  {
    try
    {
      // TODO: slow!
      final Area areaouter = areaFromRing( outerRing );
      if( innerRings != null )
      {
        for( final GM_Position[] innerRing : innerRings )
        {
          if( innerRing != null )
            areaouter.subtract( areaFromRing( innerRing ) );
        }
      }

      return areaouter;
    }
    catch( final Exception e )
    {
      Debug.debugException( e, "" );
    }

    return null;
  }

  // TODO: make static and move to helper class
  // give non-static stuff from outside
  private Area areaFromRing( final GM_Position[] ex )
  {
    final Polygon polygon = polygonFromRing( ex );
    return new Area( polygon );
  }

  // TODO: make static and move to helper class
  // give non-static stuff from outside
  private Polygon polygonFromRing( final GM_Position[] ex )
  {
    final int[] x = new int[ex.length];
    final int[] y = new int[ex.length];

    int k = 0;
    for( final GM_Position element : ex )
    {
      final GM_Position position = m_projection.getDestPoint( element );
      final int xx = (int) (position.getX() + 0.5);
      final int yy = (int) (position.getY() + 0.5);

      if( k > 0 && k < ex.length - 1 )
      {
        // PERFORMANCE: We ignore points whichs distance is smaller than 'm_width'
        // TODO: maybe even round to next smaller int? May enhance if width == 0
        if( distance( xx, yy, x[k - 1], y[k - 1] ) > m_width )
        {
          x[k] = xx;
          y[k] = yy;
          k++;
        }
      }
      else
      {
        x[k] = xx;
        y[k] = yy;
        k++;
      }
    }

    return new Polygon( x, y, k - 1 );
  }

  private static double distance( final double x1, final double y1, final double x2, final double y2 )
  {
    return Math.sqrt( (x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1) );
  }

  public void paintShape( final Graphics2D g2, final Shape shape ) throws Exception
  {
    // is completly transparent
    // if not fill polygon
    if( m_fillColor != null && m_fillColor.getAlpha() > 0 )
    {
      g2.setColor( m_fillColor );

      if( m_gFill != null )
      {
        // TODO: rotation is not considered here
        if( m_texture != null )
        {
          final Rectangle anchor = new Rectangle( 0, 0, m_texture.getWidth( null ), m_texture.getHeight( null ) );
          g2.setPaint( new TexturePaint( m_texture, anchor ) );
        }
      }

      g2.fill( shape );
    }

    if( m_strokeColor != null && m_strokeColor.getAlpha() > 0 )
    {
      g2.setColor( m_strokeColor );

      // use a simple Stroke if dash == null or dash length < 2
      BasicStroke bs2 = null;
      final float w = (float) m_width;

      if( (m_dash == null) || (m_dash.length < 2) )
      {
        bs2 = new BasicStroke( w );
      }
      else
      {
        bs2 = new BasicStroke( w, m_cap, m_join, 10.0f, m_dash, m_dashOffset );
      }

      g2.setStroke( bs2 );

      g2.draw( shape );
    }

  }

  public void paintRing( final Graphics2D gc, final GM_Position[] ring ) throws Exception
  {
    final Shape area = polygonFromRing( ring );
    paintShape( gc, area );
  }

  public void paintSurface( final Graphics2D g2, final GM_Surface< ? > surface ) throws Exception
  {
    final GM_SurfacePatch patch = surface.get( 0 );
    final GM_Position[] ex = patch.getExteriorRing();
    final GM_Position[][] inner = patch.getInteriorRings();

    final Shape shape;
    if( inner == null || inner.length == 0 )
    {
      // OPTIMIZATION: If we have no inner rings, only create a polygone, this is much faster.
      shape = polygonFromRing( ex );
    }
    else
    {
      shape = calcTargetCoordinates( ex, inner );
    }

    paintShape( g2, shape );
  }

}
