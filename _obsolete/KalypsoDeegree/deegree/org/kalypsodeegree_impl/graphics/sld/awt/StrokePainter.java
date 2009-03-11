/** This file is part of kalypso/deegree.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * history:
 * 
 * Files in this package are originally taken from deegree and modified here
 * to fit in kalypso. As goals of kalypso differ from that one in deegree
 * interface-compatibility to deegree is wanted but not retained always. 
 * 
 * If you intend to use this software in other ways than in kalypso 
 * (e.g. OGC-web services), you should consider the latest version of deegree,
 * see http://www.deegree.org .
 *
 * all modifications are licensed as deegree, 
 * original copyright:
 *
 * Copyright (C) 2001 by:
 * EXSE, Department of Geography, University of Bonn
 * http://www.giub.uni-bonn.de/exse/
 * lat/lon GmbH
 * http://www.lat-lon.de
 */
package org.kalypsodeegree_impl.graphics.sld.awt;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.geom.AffineTransform;
import java.util.ArrayList;
import java.util.List;

import org.kalypso.contribs.java.awt.ColorUtilities;
import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.graphics.sld.GraphicStroke;
import org.kalypsodeegree.graphics.sld.Stroke;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree_impl.graphics.displayelements.CurveWalker;
import org.kalypsodeegree_impl.graphics.displayelements.strokearrow.IAdditionalStrokePainter;
import org.kalypsodeegree_impl.graphics.displayelements.strokearrow.StrokeArrowHelper;
import org.kalypsodeegree_impl.graphics.displayelements.strokearrow.StrokeArrowPainter;
import org.kalypsodeegree_impl.graphics.sld.Symbolizer_Impl.UOM;

/**
 * @author Thomas Jung
 */
public class StrokePainter
{
  private final Image m_image;

  private final Color m_color;

  private final float m_width;

  private final java.awt.Stroke m_stroke;

  // additional painters - paint an arrow, aso
  private final List<IAdditionalStrokePainter> m_painters = new ArrayList<IAdditionalStrokePainter>();

  public StrokePainter( final Stroke stroke, final Feature feature, final UOM uom, final GeoTransform projection ) throws FilterEvaluationException
  {
    m_color = stroke == null ? null : ColorUtilities.createTransparent( stroke.getStroke( feature ), stroke.getOpacity( feature ) );
    m_width = stroke == null ? 0.0f : (float) stroke.getWidth( feature );

    final float[] dashArray = stroke == null ? null : stroke.getDashArray( feature );
    final float dashOffset = stroke == null ? 0.0f : stroke.getDashOffset( feature );

    final int cap = stroke == null ? BasicStroke.CAP_ROUND : stroke.getLineCap( feature );
    final int join = stroke == null ? BasicStroke.JOIN_ROUND : stroke.getLineJoin( feature );

    getAdditionalPainters( stroke, projection, uom );

    // use a simple Stroke if dash == null or its length < 2 because that's faster
    if( stroke == null )
      m_stroke = null;
    else if( dashArray == null || dashArray.length < 2 )
      m_stroke = new BasicStroke( m_width, cap, join );
    else
      m_stroke = new BasicStroke( m_width, cap, join, 10.0f, dashArray, dashOffset );

    final GraphicStroke graphicStroke = stroke == null ? null : stroke.getGraphicStroke();
    if( graphicStroke != null && uom != null && projection != null )
      m_image = graphicStroke.getGraphic().getAsImage( feature, uom, projection );
    else
      m_image = null;
  }

  private void getAdditionalPainters( Stroke stroke, GeoTransform projection, final UOM uom )
  {
    /* TODO bad style - StrokePainter shouldn't know something about IAdditionalStrokePainter Implementations */
    if( StrokeArrowHelper.isArrowDefined( stroke ) )
    {
      m_painters.add( new StrokeArrowPainter( stroke.getCssParameters(), projection, uom ) );
    }
  }

  public double getWidth( )
  {
    return m_width;
  }

  public boolean isVisible( )
  {
    return m_color != null && m_color.getAlpha() != 0;
  }

  public void prepareGraphics( final Graphics2D g2 )
  {
    // Color & Opacity
    if( m_color != null )
      g2.setColor( m_color );
    if( m_stroke != null )
      g2.setStroke( m_stroke );
  }

  /**
   * Draws the list of poses as a line according to this painter.
   */
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

    // TODO: verify if this is correct!

    final AffineTransform transform = new AffineTransform();
    transform.rotate( rotation, x, y );
    transform.translate( -image.getWidth( null ), -image.getHeight( null ) >> 1 );
    g.setTransform( transform );

    // render the image
    g.drawImage( image, x, y, null );

    // restore original transform
    g.setTransform( saveAT );
  }

  /**
   * Renders a curve to the submitted graphic context. TODO: Calculate miterlimit.
   */
  private void drawLine( final Graphics2D g, final int[][] pos )
  {
    prepareGraphics( g );

    g.drawPolyline( pos[0], pos[1], pos[2][0] );
  }

  public void paintAdditionals( Graphics2D g, GM_Curve curve, int[][] pos )
  {
    prepareGraphics( g );

    for( IAdditionalStrokePainter painter : m_painters )
    {
      painter.paint( g, curve, pos );
    }
  }
}
