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
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.geom.AffineTransform;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Iterator;

import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.graphics.displayelements.LineStringDisplayElement;
import org.kalypsodeegree.graphics.sld.LineSymbolizer;
import org.kalypsodeegree.graphics.sld.Symbolizer;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_LineString;
import org.kalypsodeegree.model.geometry.GM_MultiCurve;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.graphics.sld.LineSymbolizer_Impl;
import org.kalypsodeegree_impl.tools.Debug;

/**
 * DisplayElement that encapsulates a linestring (<tt>GM_Curve</tt>) or multi-linestring geometry (
 * <tt>GM_MultiCurve</tt>) and a <tt>LineStringSymbolizer</tt>.
 * <p>
 * It can be rendered using a solid stroke or a graphics stroke.
 * <p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider </a>
 * @version $Revision$ $Date$
 */
class LineStringDisplayElement_Impl extends GeometryDisplayElement_Impl implements LineStringDisplayElement,
    Serializable
{
  /** Use serialVersionUID for interoperability. */
  private final static long serialVersionUID = -4657962592230618248L;

  /**
   * Creates a new LineStringDisplayElement_Impl object.
   * 
   * @param feature
   * @param geometry
   */
  protected LineStringDisplayElement_Impl( Feature feature, GM_Curve geometry )
  {
    super( feature, geometry, null );

    Symbolizer defaultSymbolizer = new LineSymbolizer_Impl();
    this.setSymbolizer( defaultSymbolizer );
  }

  /**
   * Creates a new LineStringDisplayElement_Impl object.
   * 
   * @param feature
   * @param geometry
   * @param symbolizer
   */
  protected LineStringDisplayElement_Impl( Feature feature, GM_Curve geometry, LineSymbolizer symbolizer )
  {
    super( feature, geometry, symbolizer );
  }

  /**
   * Creates a new LineStringDisplayElement_Impl object.
   * 
   * @param feature
   * @param geometry
   */
  protected LineStringDisplayElement_Impl( Feature feature, GM_MultiCurve geometry )
  {
    super( feature, geometry, null );

    Symbolizer defaultSymbolizer = new LineSymbolizer_Impl();
    this.setSymbolizer( defaultSymbolizer );
  }

  /**
   * Creates a new LineStringDisplayElement_Impl object.
   * 
   * @param feature
   * @param geometry
   * @param symbolizer
   */
  protected LineStringDisplayElement_Impl( Feature feature, GM_MultiCurve geometry, LineSymbolizer symbolizer )
  {
    super( feature, geometry, symbolizer );
  }

  public void paintImage( Image image, Graphics2D g, int x, int y, double rotation )
  {

    // get the current transform
    AffineTransform saveAT = g.getTransform();

    // translation parameters (rotation)
    AffineTransform transform = new AffineTransform();
    transform.rotate( rotation, x, y );
    transform.translate( -image.getWidth( null ), -image.getHeight( null ) / 2.0 );
    g.setTransform( transform );

    // render the image
    g.drawImage( image, x, y, null );

    // restore original transform
    g.setTransform( saveAT );
  }

  /**
   * renders the DisplayElement to the submitted graphic context
   */
  public void paint( Graphics g, GeoTransform projection )
  {
    Debug.debugMethodBegin( this, "paint" );

    LineSymbolizer sym = (LineSymbolizer)symbolizer;
    org.kalypsodeegree.graphics.sld.Stroke stroke = sym.getStroke();

    // no stroke defined -> don't draw anything
    if( stroke == null )
    {
      return;
    }

    // GraphicStroke label
    if( stroke.getGraphicStroke() != null )
    {
      try
      {
        Image image = stroke.getGraphicStroke().getGraphic().getAsImage( feature );
        CurveWalker walker = new CurveWalker( g.getClipBounds() );

        if( geometry instanceof GM_Curve )
        {
          int[][] pos = LabelFactory.calcScreenCoordinates( projection, (GM_Curve)geometry );
          ArrayList positions = walker.createPositions( pos, image.getWidth( null ) );
          Iterator it = positions.iterator();
          while( it.hasNext() )
          {
            double[] label = (double[])it.next();
            int x = (int)( label[0] + 0.5 );
            int y = (int)( label[1] + 0.5 );
            paintImage( image, (Graphics2D)g, x, y, label[2] );
          }
        }
      }
      catch( Exception e )
      {
        e.printStackTrace();
      }

      // solid color label
    }
    else
    {
      try
      {
        int[][] pos = null;
        Graphics2D g2 = (Graphics2D)g;

        if( geometry instanceof GM_Curve )
        {
          pos = calcTargetCoordinates( projection, (GM_Curve)geometry );
          drawLine( g2, pos, stroke );
        }
        else
        {
          GM_MultiCurve mc = (GM_MultiCurve)geometry;
          for( int i = 0; i < mc.getSize(); i++ )
          {
            pos = calcTargetCoordinates( projection, mc.getCurveAt( i ) );
            drawLine( g2, pos, stroke );
          }
        }
      }
      catch( Exception e )
      {
        System.out.println( e );
      }
    }
    Debug.debugMethodEnd();
  }

  public double getDistance( double x1, double y1, double x2, double y2 )
  {
    double dx = x2 - x1;
    double dy = y2 - y1;
    return Math.sqrt( dx * dx + dy * dy );
  }

  /**
   * Calculates the screen coordinates of the curve.
   */
  private int[][] calcTargetCoordinates( GeoTransform projection, GM_Curve curve ) throws Exception
  {
    GM_LineString lineString = curve.getAsLineString();
    int count = lineString.getNumberOfPoints();
    int[][] pos = new int[3][];
    pos[0] = new int[count];
    pos[1] = new int[count];
    pos[2] = new int[1];

    int k = 0;
    for( int i = 0; i < count; i++ )
    {
      GM_Position position = lineString.getPositionAt( i );
      double tx = projection.getDestX( position.getX() );
      double ty = projection.getDestY( position.getY() );
      if( i > 0 )
      {
        if( distance( tx, ty, pos[0][k - 1], pos[1][k - 1] ) > 1 )
        {
          pos[0][k] = (int)( tx + 0.5 );
          pos[1][k] = (int)( ty + 0.5 );
          k++;
        }
      }
      else
      {
        pos[0][k] = (int)( tx + 0.5 );
        pos[1][k] = (int)( ty + 0.5 );
        k++;
      }
    }
    pos[2][0] = k;

    return pos;
  }

  /**
   * Renders a curve to the submitted graphic context.
   * 
   * TODO: Calculate miterlimit.
   */
  private void drawLine( Graphics g, int[][] pos, org.kalypsodeegree.graphics.sld.Stroke stroke )
      throws FilterEvaluationException
  {

    // Color & Opacity
    Graphics2D g2 = (Graphics2D)g;
    setColor( g2, stroke.getStroke( feature ), stroke.getOpacity( feature ) );

    float[] dash = stroke.getDashArray( feature );

    // use a simple Stroke if dash == null or its length < 2
    // that's faster
    float width = (float)stroke.getWidth( feature );
    int cap = stroke.getLineCap( feature );
    int join = stroke.getLineJoin( feature );
    BasicStroke bs2 = null;

    if( ( dash == null ) || ( dash.length < 2 ) )
    {
      bs2 = new BasicStroke( width, cap, join );
    }
    else
    {
      bs2 = new BasicStroke( width, cap, join, 10.0f, dash, stroke.getDashOffset( feature ) );
    }

    g2.setStroke( bs2 );

    g2.drawPolyline( pos[0], pos[1], pos[2][0] );

  }

  private double distance( double x1, double y1, double x2, double y2 )
  {
    return Math.sqrt( ( x2 - x1 ) * ( x2 - x1 ) + ( y2 - y1 ) * ( y2 - y1 ) );
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
}