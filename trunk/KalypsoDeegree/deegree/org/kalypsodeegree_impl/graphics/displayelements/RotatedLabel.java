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
package org.kalypsodeegree_impl.graphics.displayelements;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.TexturePaint;
import java.awt.font.LineMetrics;
import java.awt.geom.AffineTransform;
import java.awt.image.BufferedImage;

import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.graphics.displayelements.Label;
import org.kalypsodeegree.graphics.sld.Fill;
import org.kalypsodeegree.graphics.sld.GraphicFill;
import org.kalypsodeegree.graphics.sld.Halo;
import org.kalypsodeegree.model.feature.Feature;

/**
 * This is a rotated label with style information and screen coordinates, ready
 * to be rendered to the view.
 * <p>
 * 
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider </a>
 * @version $Revision$ $Date$
 */
class RotatedLabel implements Label
{

  private String caption;

  private int[] xpoints;

  private int[] ypoints;

  private double rotation;

  // width and height of the caption
  private int w, h;

  private Color color;

  private Font font;

  private int descent, ascent;

  private Halo halo;

  private Feature feature;

  RotatedLabel( String caption, Font font, Color color, LineMetrics metrics, Feature feature,
      Halo halo, int x, int y, int w, int h, double rotation, double anchorPoint[],
      double[] displacement )
  {

    this.caption = caption;
    this.font = font;
    this.color = color;
    this.descent = (int)metrics.getDescent();
    this.ascent = (int)metrics.getAscent();
    this.feature = feature;
    this.halo = halo;
    this.rotation = rotation;

    this.w = w;
    this.h = h;

    // vertices of label boundary
    int[] xpoints = new int[4];
    int[] ypoints = new int[4];
    xpoints[0] = x;
    ypoints[0] = y;
    xpoints[1] = x + w;
    ypoints[1] = y;
    xpoints[2] = x + w;
    ypoints[2] = y - h;
    xpoints[3] = x;
    ypoints[3] = y - h;

    // get rotated + translated points
    this.xpoints = new int[4];
    this.ypoints = new int[4];
    int tx = xpoints[0];
    int ty = ypoints[0];

    // transform all vertices of the boundary
    for( int i = 0; i < 4; i++ )
    {
      int[] point = transformPoint( xpoints[i], ypoints[i], tx, ty, rotation, anchorPoint[0],
          anchorPoint[1], w, h, displacement[0], displacement[1] );
      this.xpoints[i] = point[0];
      this.ypoints[i] = point[1];
    }
  }

  public String getCaption()
  {
    return caption;
  }

  public double getRotation()
  {
    return rotation;
  }

  public void paintBoundaries( Graphics2D g )
  {
    setColor( g, new Color( 0x888888 ), 0.5 );
    g.fillPolygon( xpoints, ypoints, xpoints.length );
    g.setColor( Color.BLACK );

    // get the current transform
    AffineTransform saveAT = g.getTransform();

    // translation parameters (rotation)
    AffineTransform transform = new AffineTransform();

    // render the text
    transform.rotate( rotation, xpoints[0], ypoints[0] );
    g.setTransform( transform );
    //g.drawString( caption, xpoints [0], ypoints [0] - descent);

    // restore original transform
    g.setTransform( saveAT );
  }

  /**
   * Renders the label (including halo) to the submitted <tt>Graphics2D</tt>
   * context.
   * <p>
   * 
   * @param g
   *          <tt>Graphics2D</tt> context to be used
   */
  public void paint( Graphics2D g )
  {

    // get the current transform
    AffineTransform saveAT = g.getTransform();

    // perform transformation
    AffineTransform transform = new AffineTransform();
    transform.rotate( rotation, xpoints[0], ypoints[0] );
    g.setTransform( transform );

    // render the halo (only if specified)
    if( halo != null )
    {
      try
      {
        paintHalo( g, halo, xpoints[0], ypoints[0] - descent );
      }
      catch( FilterEvaluationException e )
      {
        e.printStackTrace();
      }
    }

    // render the text
    setColor( g, color, 1.0 );
    g.setFont( font );
    g.drawString( caption, xpoints[0], ypoints[0] - descent );

    // restore original transform
    g.setTransform( saveAT );
  }

  /**
   * Renders the label's halo to the submitted <tt>Graphics2D</tt> context.
   * <p>
   * 
   * @param g
   *          <tt>Graphics2D</tt> context to be used
   * @param halo
   *          <tt>Halo</tt> from the SLD
   * @param x
   *          x-coordinate of the label
   * @param y
   *          y-coordinate of the label
   * 
   * @throws FilterEvaluationException
   *           if the evaluation of a <tt>ParameterValueType</tt> fails
   */
  private void paintHalo( Graphics2D g, Halo halo, int x, int y ) throws FilterEvaluationException
  {

    int radius = (int)halo.getRadius( feature );

    // only draw filled rectangle or circle, if Fill-Element is given
    Fill fill = halo.getFill();

    if( fill != null )
    {
      GraphicFill gFill = fill.getGraphicFill();

      if( gFill != null )
      {
        BufferedImage texture = gFill.getGraphic().getAsImage( feature );
        Rectangle anchor = new Rectangle( 0, 0, texture.getWidth( null ), texture.getHeight( null ) );
        g.setPaint( new TexturePaint( texture, anchor ) );
      }
      else
      {
        double opacity = fill.getOpacity( feature );
        Color color = fill.getFill( feature );
        setColor( g, color, opacity );
      }
    }
    else
    {
      g.setColor( Color.white );
    }

    // radius specified -> draw circle
    if( radius > 0 )
    {
      g.fillOval( ( x + ( w >> 1 ) ) - radius, y - ( ascent >> 1 ) - radius, radius << 1,
          radius << 1 );
    }
    // radius unspecified -> draw rectangle
    else
    {
      g.fillRect( x - 1, y - ascent - 1, w + 2, h + 2 );
    }

    // only stroke outline, if Stroke-Element is given
    org.kalypsodeegree.graphics.sld.Stroke stroke = halo.getStroke();

    if( stroke != null )
    {
      double opacity = stroke.getOpacity( feature );

      if( opacity > 0.01 )
      {
        Color color = stroke.getStroke( feature );
        int alpha = (int)Math.round( opacity * 255 );
        int red = color.getRed();
        int green = color.getGreen();
        int blue = color.getBlue();
        color = new Color( red, green, blue, alpha );
        g.setColor( color );

        float[] dash = stroke.getDashArray( feature );

        // use a simple Stroke if dash == null or dash length < 2
        BasicStroke bs = null;
        float strokeWidth = (float)stroke.getWidth( feature );

        if( ( dash == null ) || ( dash.length < 2 ) )
        {
          bs = new BasicStroke( strokeWidth );
        }
        else
        {
          bs = new BasicStroke( strokeWidth, stroke.getLineCap( feature ), stroke
              .getLineJoin( feature ), 10.0f, dash, stroke.getDashOffset( feature ) );
          bs = new BasicStroke( strokeWidth, stroke.getLineCap( feature ), stroke
              .getLineJoin( feature ), 1.0f, dash, 1.0f );
        }

        g.setStroke( bs );

        // radius specified -> draw circle
        if( radius > 0 )
        {
          g.drawOval( ( x + ( w >> 1 ) ) - radius, y - ( ascent >> 1 ) - radius, radius << 1,
              radius << 1 );
        }// radius unspecified -> draw rectangle
        else
        {
          g.drawRect( x - 1, y - ascent - 1, w + 2, h + 2 );
        }
      }
    }
  }

  public int getX()
  {
    return xpoints[0];
  }

  public int getY()
  {
    return ypoints[0];
  }

  public int getMaxX()
  {
    return xpoints[1];
  }

  public int getMaxY()
  {
    return ypoints[1];
  }

  public int getMinX()
  {
    return xpoints[3];
  }

  public int getMinY()
  {
    return ypoints[3];
  }

  /**
   * Determines if the label intersects with another label.
   * <p>
   * 
   * @param that
   *          label to test
   * @return true if the labels intersect
   */
  public boolean intersects( Label that )
  {
    System.out.println( "Intersection test for rotated labels is " + "not implemented yet!" );
    return false;
  }

  private int[] transformPoint( int x, int y, int tx, int ty, double rotation, double anchorPointX,
      double anchorPointY, int w, int h, double displacementX, double displacementY )
  {

    double cos = Math.cos( rotation );
    double sin = Math.sin( rotation );
    double dx = -anchorPointX * w;
    //		double dy = anchorPointY * h;
    double dy = anchorPointY * h - displacementY;

    double m00 = cos;
    double m01 = -sin;
    double m02 = cos * dx - sin * dy + tx - tx * cos + ty * sin;
    double m10 = sin;
    double m11 = cos;
    double m12 = sin * dx + cos * dy + ty - tx * sin - ty * cos;

    int[] point2 = new int[2];

    point2[0] = (int)( m00 * x + m01 * y + m02 + 0.5 + displacementX );
    point2[1] = (int)( m10 * x + m11 * y + m12 + 0.5 );
    //		point2 [1] = (int) (m10 * x + m11 * y + m12 + 0.5 - displacementY);
    return point2;
  }

  private Graphics2D setColor( Graphics2D g2, Color color, double opacity )
  {
    if( opacity < 0.999 )
    {
      final int alpha = (int)Math.round( opacity * 255 );
      final int red = color.getRed();
      final int green = color.getGreen();
      final int blue = color.getBlue();
      color = new Color( red, green, blue, alpha );
    }

    g2.setColor( color );
    return g2;
  }

  public String toString()
  {
    return caption;
  }
}