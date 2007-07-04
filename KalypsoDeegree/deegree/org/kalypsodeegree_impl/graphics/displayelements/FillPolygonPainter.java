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

  private final Stroke m_stroke;

  private final Fill m_fill;

  private float m_width;

  private double m_strokeOpacity;

  private double m_fillOpacity;

  private Color m_fillColor;

  private Color m_strokeColor;

  private GraphicFill m_gFill;

  private BufferedImage m_texture;

  private float[] m_dash;

  private int m_cap;

  private int m_join;

  private float m_dashOffset;

  private GeoTransform m_projection;

  public FillPolygonPainter( Fill fill, Stroke stroke, Feature feature, UOM uom, GeoTransform projection ) throws FilterEvaluationException
  {

    m_fill = fill;

    m_stroke = stroke;

    m_projection = projection;

    if( stroke != null )
    {
      m_width = (float) stroke.getWidth( feature );

      m_strokeOpacity = stroke.getOpacity( feature );

      m_strokeColor = m_stroke.getStroke( feature );
      m_dash = stroke.getDashArray( feature );

      m_cap = stroke.getLineCap( feature );

      m_join = stroke.getLineJoin( feature );

      m_dashOffset = stroke.getDashOffset( feature );

    }

    m_fillColor = m_fill.getFill( feature );

    m_fillOpacity = fill.getOpacity( feature );

    m_gFill = fill.getGraphicFill();
    if( m_gFill != null )
      m_texture = m_gFill.getGraphic().getAsImage( feature, uom, projection );

  }

  public <T extends GM_SurfacePatch> Area calcTargetCoordinates( final GM_Surface<T> surface ) throws Exception
  {
    try
    {
      final GM_SurfacePatch patch = surface.get( 0 );
      final GM_Position[] ex = patch.getExteriorRing();
      final GM_Position[][] inner = patch.getInteriorRings();

      final Area areaouter = areaFromRing( ex );
      if( inner != null )
      {
        for( final GM_Position[] innerRing : inner )
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

  public Area areaFromRing( final GM_Position[] ex )
  {
    float width = 0;
    if( m_stroke != null )
    {
      width = 0;
    }

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
        if( distance( xx, yy, x[k - 1], y[k - 1] ) > width )
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

    final Polygon polygon = new Polygon( x, y, k - 1 );
    return new Area( polygon );
  }

  private double distance( final double x1, final double y1, final double x2, final double y2 )
  {
    return Math.sqrt( (x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1) );
  }

  public void paintShape( final Graphics2D g2, Shape shape ) throws Exception
  {

    if( m_fill != null )
    {
      // is completly transparent
      // if not fill polygon
      if( m_fillOpacity > 0.01 )
      {
        Color color = m_fillColor;
        final int alpha = (int) Math.round( m_fillOpacity * 255 );
        final int red = color.getRed();
        final int green = color.getGreen();
        final int blue = color.getBlue();
        color = new Color( red, green, blue, alpha );

        g2.setColor( color );

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
    }

    // only stroke outline, if Stroke-Element is given
    if( m_stroke != null )
    {
      if( m_strokeOpacity > 0.01 )
      {
        Color color = m_strokeColor;
        final int alpha = (int) Math.round( m_strokeOpacity * 255 );
        final int red = color.getRed();
        final int green = color.getGreen();
        final int blue = color.getBlue();
        color = new Color( red, green, blue, alpha );

        g2.setColor( color );

        // use a simple Stroke if dash == null or dash length < 2
        BasicStroke bs2 = null;
        final float w = m_width;

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

  }

  public void paintRing( Graphics2D gc, GM_Position[] ring ) throws Exception
  {
    final Area area = areaFromRing( ring );
    paintShape( gc, area );
  }

  public void paintSurface( Graphics2D g2, GM_Surface< ? > element ) throws Exception
  {
    final Shape area = calcTargetCoordinates( element );
    paintShape( g2, area );
  }

}
