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
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.geom.AffineTransform;
import java.util.List;

import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.graphics.sld.GraphicStroke;
import org.kalypsodeegree.graphics.sld.Stroke;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.graphics.sld.Symbolizer_Impl.UOM;

/**
 * @author Thomas Jung
 */
public class StrokeLinePainter
{
  private final Stroke m_stroke;

  private Image m_image;

  private double m_opacity;

  private Color m_color;

  private float[] m_dashArray;

  private float m_width;

  private int m_cap;

  private int m_join;

  private float m_dashOffset;

  public StrokeLinePainter( final Stroke stroke, final Feature feature, final UOM uom, final GeoTransform projection ) throws FilterEvaluationException
  {
    m_stroke = stroke;

    m_color = stroke.getStroke( feature );
    m_opacity = stroke.getOpacity( feature );
    
    m_dashArray = m_stroke.getDashArray( feature );
    m_dashOffset = stroke.getDashOffset( feature );
    
    m_width = (float) stroke.getWidth( feature );
    m_cap = stroke.getLineCap( feature );
    m_join = stroke.getLineJoin( feature );

    final GraphicStroke graphicStroke = stroke == null ? null : stroke.getGraphicStroke();
    m_image = graphicStroke == null ? null : graphicStroke.getGraphic().getAsImage( feature, uom, projection );
  }

  public void paintPoses( final Graphics2D g2, final int[][] pos ) 
  {
    if( m_image != null )
    {
      final CurveWalker walker = new CurveWalker( g2.getClipBounds() );
      final List<double[]> positions = walker.createPositions( pos, m_image.getWidth( null ) );
      for( final double[] label : positions )
      {
        final int x = (int) (label[0] + 0.5);
        final int y = (int) (label[1] + 0.5);
        paintImage( m_image, g2, x, y, label[2] );
      }
    }
    else
    {
      drawLine( g2, pos );
    }
  }
  
  private static void paintImage( final Image image, final Graphics2D g, final int x, final int y, final double rotation )
  {
    // get the current transform
    final AffineTransform saveAT = g.getTransform();

    // translation parameters (rotation)
    final AffineTransform transform = new AffineTransform();
    transform.rotate( rotation, x, y );
    transform.translate( -image.getWidth( null ), -image.getHeight( null ) / 2.0 );
    g.setTransform( transform );

    // render the image
    g.drawImage( image, x, y, null );

    // restore original transform
    g.setTransform( saveAT );
  }

  /**
   * Renders a curve to the submitted graphic context. TODO: Calculate miterlimit.
   */
  private void drawLine( final Graphics g, final int[][] pos )
  {
    // Color & Opacity
    final Graphics2D g2 = (Graphics2D) g;
    setColor( g2, m_color, m_opacity );

    // use a simple Stroke if dash == null or its length < 2
    // that's faster
    final BasicStroke bs2;
    if( m_dashArray == null || m_dashArray.length < 2 )
    {
      bs2 = new BasicStroke( m_width, m_cap, m_join );
    }
    else
    {
      bs2 = new BasicStroke( m_width, m_cap, m_join, 10.0f, m_dashArray, m_dashOffset );
    }

    g2.setStroke( bs2 );
    g2.drawPolyline( pos[0], pos[1], pos[2][0] );
  }

  private static void setColor( final Graphics2D g2, Color color, final double opacity )
  {
    if( opacity < 0.999 )
    {
      final int alpha = (int) Math.round( opacity * 255 );
      final int red = color.getRed();
      final int green = color.getGreen();
      final int blue = color.getBlue();
      color = new Color( red, green, blue, alpha );
    }

    g2.setColor( color );
  }
  
}
