/*--------------- Kalypso-Deegree-Header ------------------------------------------------------------

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


 history:

 Files in this package are originally taken from deegree and modified here
 to fit in kalypso. As goals of kalypso differ from that one in deegree
 interface-compatibility to deegree is wanted but not retained always.

 If you intend to use this software in other ways than in kalypso
 (e.g. OGC-web services), you should consider the latest version of deegree,
 see http://www.deegree.org .

 all modifications are licensed as deegree,
 original copyright:

 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon GmbH
 http://www.lat-lon.de

 ---------------------------------------------------------------------------------------------------*/
package org.kalypsodeegree_impl.graphics.sld;

import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.image.BufferedImage;

import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.graphics.Resource;
import org.eclipse.swt.graphics.Transform;
import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.graphics.sld.Fill;
import org.kalypsodeegree.graphics.sld.Mark;
import org.kalypsodeegree.graphics.sld.Stroke;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.xml.Marshallable;
import org.kalypsodeegree_impl.graphics.sld.awt.FillPainter;
import org.kalypsodeegree_impl.graphics.sld.awt.StrokePainter;
import org.kalypsodeegree_impl.tools.Debug;

/**
 * A Mark takes a "shape" and applies coloring to it. The shape can be derived either from a well-known name (such as
 * "square"), an external URL in various formats (such as, perhaps GIF), or from a glyph of a font. Multiple external
 * formats may be used with the semantic that they all contain the equivalent shape in different formats. If an image
 * format is used that has inherent coloring, the coloring is discarded and only the opacity channel (or equivalent) is
 * used. A Halo, Fill, and/or Stroke is applied as appropriate for the shape's source format.
 * <p>
 * 
 * @author <a href="mailto:k.lupp@web.de">Katharina Lupp </a>
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider </a>
 * @version $Revision$ $Date$
 */
public class Mark_Impl implements Mark, Marshallable
{
  private Fill m_fill = null;

  private String m_wellKnownName = "square";

  private Stroke m_stroke = null;

  private BufferedImage m_image;

  /**
   * Constructor for the default <tt>Mark</tt>.
   */
  Mark_Impl( )
  {
    //
  }

  /**
   * constructor initializing the class with the <Mark>
   */
  Mark_Impl( final String wellKnownName, final Stroke stroke, final Fill fill )
  {
    setWellKnownName( wellKnownName );
    setStroke( stroke );
    setFill( fill );
  }

  /**
   * Gives the well known name of a Mark's shape. Allowed values include at least "square", "circle", "triangle",
   * "star", "cross", and "x", though map servers may draw a different symbol instead if they don't have a shape for all
   * of these. Renderings of these marks may be made solid or hollow depending on Fill and Stroke parameters. The
   * default value is "square".
   * 
   * @return the WK-Name of the mark
   */
  public String getWellKnownName( )
  {
    return m_wellKnownName;
  }

  /**
   * Sets the well known name of a Mark's shape. Allowed values include at least "square", "circle", "triangle", "star",
   * "cross", and "x", though map servers may draw a different symbol instead if they don't have a shape for all of
   * these. Renderings of these marks may be made solid or hollow depending on Fill and Stroke parameters. The default
   * value is "square".
   * 
   * @param wellKnownName
   *            the WK-Name of the mark
   */
  public void setWellKnownName( final String wellKnownName )
  {
    m_wellKnownName = wellKnownName;
  }

  /**
   * A Fill allows area geometries to be filled. There are two types of fills: solid-color and repeated GraphicFill. In
   * general, if a Fill element is omitted in its containing element, no fill will be rendered. The default is a solid
   * 50%-gray (color "#808080") opaque fill.
   * 
   * @return the fill of the mark
   */
  public Fill getFill( )
  {
    return m_fill;
  }

  /**
   * sets the <Fill>
   * 
   * @param fill
   *            the fill of the mark
   */
  public void setFill( final Fill fill )
  {
    m_fill = fill;
  }

  /**
   * A Stroke allows a string of line segments (or any linear geometry) to be rendered. There are three basic types of
   * strokes: solid Color, GraphicFill (stipple), and repeated GraphicStroke. A repeated graphic is plotted linearly and
   * has its graphic symbol bended around the curves of the line string. The default is a solid black line (Color
   * "#000000").
   * 
   * @return the stroke of the mark
   */
  public Stroke getStroke( )
  {
    return m_stroke;
  }

  /**
   * sets <Stroke>
   * 
   * @param stroke
   *            the stroke of the mark
   */
  public void setStroke( final Stroke stroke )
  {
    m_stroke = stroke;
  }

  /**
   * @see org.kalypsodeegree.graphics.sld.Mark#getAsImage(org.kalypsodeegree.model.feature.Feature, int)
   */
  public BufferedImage getAsImage( final Feature feature, final int size ) throws FilterEvaluationException
  {
    if( m_image == null )
    {
      m_image = new BufferedImage( size + 1, size + 1, BufferedImage.TYPE_INT_ARGB );
      final Graphics2D g2D = m_image.createGraphics();
      g2D.setRenderingHint( RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON );

      paintAwt( g2D, feature, size );

      g2D.dispose();
    }

    return m_image;
  }

  /**
   * @see org.kalypsodeegree.graphics.sld.Mark#paintAwt(java.awt.Graphics2D, org.kalypsodeegree.model.feature.Feature,
   *      int)
   */
  public void paintAwt( final Graphics2D g, final Feature feature, final int size ) throws FilterEvaluationException
  {
    // REMARK: create Stroke-Helper without uom and projection: so external graphics are not supported
    final StrokePainter strokePainter = new StrokePainter( m_stroke, feature, null, null );
    final FillPainter fillPainter = new FillPainter( m_fill, feature, null, null );

    drawImage( g, size, strokePainter, fillPainter );
  }

  private void drawImage( final Graphics2D g2D, final int size, final StrokePainter strokePainter, final FillPainter fillPainter )
  {
    if( m_wellKnownName.equalsIgnoreCase( "circle" ) )
    {
      drawCircle( g2D, size, strokePainter, fillPainter );
    }
    else if( m_wellKnownName.equalsIgnoreCase( "triangle" ) )
    {
      drawTriangle( g2D, size, strokePainter, fillPainter );
    }
    else if( m_wellKnownName.equalsIgnoreCase( "cross" ) )
    {
      drawCross1( g2D, size, strokePainter );
    }
    else if( m_wellKnownName.equalsIgnoreCase( "x" ) )
    {
      drawCross2( g2D, size, strokePainter );
    }
    /* Kalypso known names */
    else if( m_wellKnownName.equalsIgnoreCase( "kalypsoArrow" ) )
    {
      drawArrow( g2D, size, strokePainter, fillPainter );
    }
    else
    {
      drawSquare( g2D, size, strokePainter, fillPainter );
    }
  }

  /**
   * @see org.kalypsodeegree.graphics.sld.Mark#paint(org.kalypsodeegree.model.feature.Feature)
   */
  public void paint( final GC gc, final Feature feature ) throws FilterEvaluationException
  {
    final Resource[] strokeResources = LineSymbolizer_Impl.prepareGc( gc, m_stroke, feature );
    final Resource[] fillResources = PolygonSymbolizer_Impl.prepareGc( gc, m_fill, feature );

    // TODO: stroke line width is not yet supported with the awt stuff, so set always to 1 here
// gc.setLineWidth( 1 );

    final Transform oldTrans = new Transform( gc.getDevice() );
    gc.getTransform( oldTrans );
    try
    {
      if( m_wellKnownName == null )
        m_wellKnownName = "square";

      final Rectangle clipping = gc.getClipping();
      final float clipSize = clipping.width;
      final float size = clipSize / 2;

      // HACK: in order to have a nice legend, we paint it only half sized
      // this must be removed if this code is ever used to paint the real features

      final Transform trans = new Transform( gc.getDevice() );

      trans.scale( 0.8f, 0.8f );
      trans.translate( size, size );
// trans.translate( -size, -size );

      gc.setTransform( trans );

      if( m_wellKnownName.equalsIgnoreCase( "circle" ) )
        drawCircle( gc, (int) size );
      else if( m_wellKnownName.equalsIgnoreCase( "triangle" ) )
        drawTriangle( gc, (int) size );
      else if( m_wellKnownName.equalsIgnoreCase( "cross" ) )
        drawCross1( gc, (int) size );
      else if( m_wellKnownName.equalsIgnoreCase( "x" ) )
        drawCross2( gc, (int) size );
      /* Kalypso known names */
      else if( m_wellKnownName.equalsIgnoreCase( "kalypsoArrow" ) )
        drawArrow( gc, (int) size );
      else
        drawSquare( gc, (int) size );
    }
    finally
    {
      Symbolizer_Impl.disposeResource( fillResources );
      Symbolizer_Impl.disposeResource( strokeResources );

      gc.setTransform( oldTrans );
    }
  }

  /**
   * Draws a scaled instance of a triangle mark according to the given parameters.
   * 
   * @param size
   *            resulting image's height and widthh
   * @param fillColor
   *            <tt>Color</tt> to be used for the fill
   * @param strokeColor
   *            <tt>Color</tt> to be used for the strokes
   * @return image displaying a triangle
   */
  public void drawTriangle( final Graphics2D g2D, final int size, final StrokePainter strokePainter, final FillPainter fillPainter )
  {
    final int[] x_ = new int[3];
    final int[] y_ = new int[3];
    x_[0] = 0;
    y_[0] = 0;
    x_[1] = size / 2;
    y_[1] = size - 1;
    x_[2] = size - 1;
    y_[2] = 0;

    if( fillPainter.isVisible() )
    {
      fillPainter.prepareGraphics( g2D );
      g2D.fillPolygon( x_, y_, 3 );
    }

    if( strokePainter.isVisible() )
    {
      strokePainter.prepareGraphics( g2D );
      g2D.drawPolygon( x_, y_, 3 );
    }
  }

  /**
   * Draws a scaled instance of a triangle mark according to the given parameters.
   * 
   * @param size
   *            resulting image's height and widthh
   */
  public void drawTriangle( final GC gc, final int size )
  {
    final int[] points = new int[8];

    points[0] = 0;
    points[1] = 0;
    points[2] = size / 2;
    points[3] = size - 1;
    points[4] = size - 1;
    points[5] = 0;
    points[6] = 0;
    points[7] = 0;

    gc.fillPolygon( points );
    gc.drawPolygon( points );
  }

  /**
   * Draws a scaled instance of a circle mark according to the given parameters.
   * 
   * @param size
   *            resulting image's height and widthh
   * @param fillColor
   *            <tt>Color</tt> to be used for the fill
   * @param strokeColor
   *            <tt>Color</tt> to be used for the strokes
   * @return image displaying a circle
   */
  public void drawCircle( final Graphics2D g2D, final int size, final StrokePainter strokePainter, final FillPainter fillPainter )
  {
    if( fillPainter.isVisible() )
    {
      fillPainter.prepareGraphics( g2D );
      g2D.fillOval( 0, 0, size, size );
    }

    if( strokePainter.isVisible() )
    {
      strokePainter.prepareGraphics( g2D );
      g2D.drawOval( 0, 0, size, size );
    }
  }

  /**
   * Draws a scaled instance of a circle mark according to the given parameters.
   * 
   * @param size
   *            resulting image's height and widthh
   */
  public void drawCircle( final GC gc, final int size )
  {
    gc.fillOval( 0, 0, size, size );
    gc.drawOval( 0, 0, size, size );
  }

  /**
   * Draws a scaled instance of a square mark according to the given parameters.
   * 
   * @param size
   *            resulting image's height and widthh
   * @param fillColor
   *            <tt>Color</tt> to be used for the fill
   * @param strokeColor
   *            <tt>Color</tt> to be used for the strokes
   */
  public void drawSquare( final Graphics2D g2D, final int size, final StrokePainter strokePainter, final FillPainter fillPainter )
  {
    if( fillPainter.isVisible() )
    {
      fillPainter.prepareGraphics( g2D );
      g2D.fillRect( 0, 0, size, size );
    }

    if( strokePainter.isVisible() )
    {
      strokePainter.prepareGraphics( g2D );
      g2D.drawRect( 0, 0, size, size );
    }
  }

  /**
   * Draws a scaled instance of a square mark according to the given parameters.
   * 
   * @param size
   *            resulting image's height and widthh
   */
  public void drawSquare( final GC gc, final int size )
  {
    gc.fillRectangle( 0, 0, size, size );
    gc.drawRectangle( 0, 0, size, size );
  }

  /**
   * Draws a scaled instance of a cross mark (a "+") according to the given parameters.
   * 
   * @param size
   *            resulting image's height and width
   * @param strokeColor
   *            <tt>Color</tt> to be used for the strokes
   * @return image displaying a cross (a "+")
   */
  public void drawCross1( final Graphics2D g2D, final int size, final StrokePainter strokePainter )
  {
    if( strokePainter.isVisible() )
    {
      strokePainter.prepareGraphics( g2D );
      g2D.drawLine( 0, size / 2, size - 1, size / 2 );
      g2D.drawLine( size / 2, 0, size / 2, size - 1 );
    }
  }

  /**
   * Draws a scaled instance of a cross mark (a "+") according to the given parameters.
   * 
   * @param size
   *            resulting image's height and widthh
   */
  public void drawCross1( final GC gc, final int size )
  {
    gc.drawLine( 0, size / 2, size - 1, size / 2 );
    gc.drawLine( size / 2, 0, size / 2, size - 1 );
  }

  /**
   * Draws a scaled instance of a cross mark (an "X") according to the given parameters.
   * 
   * @param size
   *            resulting image's height and widthh
   * @param strokeColor
   *            <tt>Color</tt> to be used for the strokes
   * @return image displaying a cross (a "X")
   */
  public void drawCross2( final Graphics2D g2D, final int size, final StrokePainter strokePainter )
  {
    if( strokePainter.isVisible() )
    {
      strokePainter.prepareGraphics( g2D );
      g2D.drawLine( 0, 0, size - 1, size - 1 );
      g2D.drawLine( 0, size - 1, size - 1, 0 );
    }
  }

  /**
   * Draws a scaled instance of a cross mark (an "X") according to the given parameters.
   * 
   * @param size
   *            resulting image's height and widthh
   */
  public void drawCross2( final GC gc, final int size )
  {
    gc.drawLine( 0, 0, size - 1, size - 1 );
    gc.drawLine( 0, size - 1, size - 1, 0 );
  }

  /**
   * Draws a scaled instance of a triangle mark according to the given parameters.
   * <p>
   * The arrow start at the middle of the rectangle and points right, the arrow-tip touching the border.
   * 
   * @param size
   *            resulting image's height and widthh
   * @param fillColor
   *            <tt>Color</tt> to be used for the fill
   * @param strokeColor
   *            <tt>Color</tt> to be used for the strokes
   * @return image displaying a triangle
   */
  public void drawArrow( final Graphics2D g2D, final int size, final StrokePainter strokePainter, final FillPainter fillPainter )
  {
    final int middle = size / 2;
    final int triangleBottom = size - size / 8;
    final int triangleLeft = middle - size / 16;
    final int triangleRight = middle + size / 16;
    final int triangleTop = size;

    /* Draw the triangle */
    final int[] x_ = new int[3];
    final int[] y_ = new int[3];
    x_[0] = triangleLeft;
    y_[0] = triangleBottom;
    x_[1] = middle;
    y_[1] = triangleTop;
    x_[2] = triangleRight;
    y_[2] = triangleBottom;

    // REMARK: we switch x and y in order to paint the arrow from left to right
    if( fillPainter.isVisible() )
    {
      fillPainter.prepareGraphics( g2D );
      g2D.fillPolygon( y_, x_, 3 );
    }

    if( strokePainter.isVisible() )
    {
      strokePainter.prepareGraphics( g2D );
      g2D.drawPolygon( y_, x_, 3 );
      g2D.drawLine( middle, middle, triangleBottom, middle );
    }
  }

  /**
   * Draws a scaled instance of a triangle mark according to the given parameters.
   * 
   * @param size
   *            resulting image's height and widthh
   */
  public void drawArrow( final GC gc, final int size )
  {
    final int middle = size / 2;
    final int triangleBottom = size - size / 8;
    final int triangleLeft = middle - size / 16;
    final int triangleRight = middle + size / 16;
    final int triangleTop = size;

    /* Draw the triangle */
    // REMARK: we switch x and y in order to paint the arrow from left to right
    final int[] points = new int[8];
    points[0] = triangleBottom;
    points[1] = triangleLeft;
    points[2] = triangleTop;
    points[3] = middle;
    points[4] = triangleBottom;
    points[5] = triangleRight;
    points[6] = triangleBottom;
    points[7] = triangleLeft;

    gc.fillPolygon( points );
    gc.drawPolygon( points );

    /* Draw the line */
    gc.drawLine( middle, middle, triangleBottom, middle );
  }

  /**
   * exports the content of the Mark as XML formated String
   * 
   * @return xml representation of the Mark
   */
  public String exportAsXML( )
  {
    Debug.debugMethodBegin();

    final StringBuffer sb = new StringBuffer( 1000 );
    sb.append( "<Mark>" );
    if( m_wellKnownName != null && !m_wellKnownName.equals( "" ) )
    {
      sb.append( "<WellKnownName>" ).append( m_wellKnownName );
      sb.append( "</WellKnownName>" );
    }
    if( m_fill != null )
    {
      sb.append( ((Marshallable) m_fill).exportAsXML() );
    }
    if( m_stroke != null )
    {
      sb.append( ((Marshallable) m_stroke).exportAsXML() );
    }

    sb.append( "</Mark>" );

    Debug.debugMethodEnd();
    return sb.toString();
  }
}