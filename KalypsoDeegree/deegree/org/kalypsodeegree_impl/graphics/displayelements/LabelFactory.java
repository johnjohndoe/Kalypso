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

import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.font.FontRenderContext;
import java.awt.font.LineMetrics;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.graphics.displayelements.Label;
import org.kalypsodeegree.graphics.displayelements.LabelDisplayElement;
import org.kalypsodeegree.graphics.sld.Halo;
import org.kalypsodeegree.graphics.sld.LabelPlacement;
import org.kalypsodeegree.graphics.sld.LinePlacement;
import org.kalypsodeegree.graphics.sld.PointPlacement;
import org.kalypsodeegree.graphics.sld.TextSymbolizer;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_LineString;
import org.kalypsodeegree.model.geometry.GM_MultiCurve;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.tools.Debug;

/**
 * Does the labeling, i.e. creates (screen) <tt>Label</tt> representations from <tt>LabelDisplayElement</tt>s.
 * <p>
 * Different geometry-types (of the LabelDisplayElement) imply different strategies concerning the way the
 * <tt>Labels</tt> are generated.
 * <p>
 * 
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider </a>
 * @version $Revision$ $Date$
 */
public class LabelFactory
{

  public static Label createLabel( final String caption, final Font font, final Color color, final LineMetrics metrics, final Feature feature, final Halo halo, final int x, final int y, final int w, final int h, final double rotation, final double anchorPointX, final double anchorPointY, final double displacementX, final double displacementY )
  {
    if( rotation == 0.0 )
    {
      return new HorizontalLabel( caption, font, color, metrics, feature, halo, x, y, w, h, new double[] { anchorPointX, anchorPointY }, new double[] { displacementX, displacementY } );
    }
    return new RotatedLabel( caption, font, color, metrics, feature, halo, x, y, w, h, rotation, new double[] { anchorPointX, anchorPointY }, new double[] { displacementX, displacementY } );
  }

  public static Label createLabel( final String caption, final Font font, final Color color, final LineMetrics metrics, final Feature feature, final Halo halo, final int x, final int y, final int w, final int h, final double rotation, final double[] anchorPoint, final double[] displacement )
  {
    if( rotation == 0.0 )
    {
      return new HorizontalLabel( caption, font, color, metrics, feature, halo, x, y, w, h, anchorPoint, displacement );
    }
    return new RotatedLabel( caption, font, color, metrics, feature, halo, x, y, w, h, rotation, anchorPoint, displacement );
  }

  /**
   * Generates <tt>Label</tt> -representations for a given <tt>LabelDisplayElement</tt>.
   * <p>
   * 
   * @throws Exception
   */
  public static Label[] createLabels( final LabelDisplayElement element, final GeoTransform projection, final Graphics2D g ) throws Exception
  {
    Label[] labels = new Label[0];
    final Feature feature = element.getFeature();

    String caption = null;
    try
    {
      caption = element.getLabel().evaluate( feature );
    }
    catch( final FilterEvaluationException e )
    {
      // if properties are unknown to features, this should be ignored!
      return labels;
    }
    // sanity check: empty labels are ignored
    if( caption == null || caption.trim().equals( "" ) )
    {
      return labels;
    }

    final TextSymbolizer symbolizer = (TextSymbolizer) element.getSymbolizer();

    // gather font information
    final org.kalypsodeegree.graphics.sld.Font sldFont = symbolizer.getFont();
    final java.awt.Font font = new java.awt.Font( sldFont.getFamily( feature ), sldFont.getStyle( feature ) | sldFont.getWeight( feature ), sldFont.getSize( feature ) );
    g.setFont( font );
    final FontRenderContext frc = g.getFontRenderContext();

    // bugstories...
    // I got the follwoing error in the next line:
    // # An unexpected error has been detected by HotSpot Virtual Machine:
    // # [...]
    // # Problematic frame:
    // # C [libfontmanager.so+0x2ecd5]
    //
    // while running kalypso on linux, kubuntu-Dapper.
    // The error was caused by some buggy fonts in Dapper (Rekha-normal and aakar-MagNet ).
    // Work-around is to remove the toxic fonts by removing the package ttf-gujarati-fonts from the distribution.
    // this error was not easy to locate, so do not remove this notice !
    // ( v.doemming@tuhh.de )

    final Rectangle2D bounds = font.getStringBounds( caption, frc );
    final LineMetrics metrics = font.getLineMetrics( caption, frc );
    final int w = (int) bounds.getWidth();
    final int h = (int) bounds.getHeight();
    // int descent = (int) metrics.getDescent ();

    final GM_Object[] geometries = element.getGeometry();
    for( final GM_Object geometry : geometries )
      labels = createabels( element, projection, g, labels, feature, caption, geometry, symbolizer, sldFont, font, metrics, w, h );
    return labels;
  }

  private static Label[] createabels( final LabelDisplayElement element, final GeoTransform projection, final Graphics2D g, Label[] labels, final Feature feature, final String caption, final GM_Object geometry, final TextSymbolizer symbolizer, final org.kalypsodeegree.graphics.sld.Font sldFont, final java.awt.Font font, final LineMetrics metrics, final int w, final int h ) throws FilterEvaluationException, GM_Exception, Exception
  {
    if( geometry instanceof GM_Point )
    {

      // get screen coordinates
      final int[] coords = calcScreenCoordinates( projection, geometry );
      final int x = coords[0];
      final int y = coords[1];

      // default placement information
      double rotation = 0.0;
      double[] anchorPoint = { 0.0, 0.5 };
      double[] displacement = { 0.0, 0.0 };

      // use placement information from SLD
      final LabelPlacement lPlacement = symbolizer.getLabelPlacement();

      if( lPlacement != null )
      {
        final PointPlacement pPlacement = lPlacement.getPointPlacement();
        anchorPoint = pPlacement.getAnchorPoint( feature );
        displacement = pPlacement.getDisplacement( feature );
        rotation = pPlacement.getRotation( feature );
      }

      labels = new Label[] { createLabel( caption, font, sldFont.getColor( feature ), metrics, feature, symbolizer.getHalo(), x, y, w, h, rotation * Math.PI, anchorPoint, displacement ) };
    }
    else if( geometry instanceof GM_Surface || geometry instanceof GM_MultiSurface )
    {

      // get screen coordinates
      final int[] coords = calcScreenCoordinates( projection, geometry );
      int x = coords[0];
      int y = coords[1];

      // default placement information
      double rotation = 0.0;
      double[] anchorPoint = { 0.5, 0.5 };
      double[] displacement = { 0.0, 0.0 };

      // use placement information from SLD
      final LabelPlacement lPlacement = symbolizer.getLabelPlacement();

      if( lPlacement != null )
      {
        final PointPlacement pPlacement = lPlacement.getPointPlacement();

        // check if the label is to be centered within the intersection of
        // the screen surface and the polygon geometry
        if( pPlacement.isAuto() )
        {
          final GM_Surface<GM_SurfacePatch> screenSurface = GeometryFactory.createGM_Surface( projection.getSourceRect(), null );
          final GM_Object intersection = screenSurface.intersection( geometry );
          if( intersection != null )
          {
            final GM_Position source = intersection.getCentroid().getPosition();
            x = (int) (projection.getDestX( source.getX() ) + 0.5);
            y = (int) (projection.getDestY( source.getY() ) + 0.5);
          }
        }
        anchorPoint = pPlacement.getAnchorPoint( feature );
        displacement = pPlacement.getDisplacement( feature );
        rotation = pPlacement.getRotation( feature );
      }

      labels = new Label[] { createLabel( caption, font, sldFont.getColor( feature ), metrics, feature, symbolizer.getHalo(), x, y, w, h, rotation * Math.PI, anchorPoint, displacement )

      };
    }
    else if( geometry instanceof GM_Curve || geometry instanceof GM_MultiCurve )
    {
      final GM_Surface<GM_SurfacePatch> screenSurface = GeometryFactory.createGM_Surface( projection.getSourceRect(), null );
      final GM_Object intersection = screenSurface.intersection( geometry );
      if( intersection != null )
      {
        List<Label> list = null;
        if( intersection instanceof GM_Curve )
        {
          list = createLabels( (GM_Curve) intersection, element, g, projection );
        }
        else if( intersection instanceof GM_MultiCurve )
        {
          list = createLabels( (GM_MultiCurve) intersection, element, g, projection );
        }
        else
        {
          throw new Exception( "Intersection produced unexpected geometry type: '" + intersection.getClass().getName() + "'!" );
        }
        labels = new Label[list.size()];
        for( int i = 0; i < labels.length; i++ )
        {
          final Label label = list.get( i );
          labels[i] = label;
        }
      }
    }
    else
    {
      throw new Exception( "LabelFactory does not implement generation " + "of Labels from geometries of type: '" + geometry.getClass().getName() + "'!" );
    }
    return labels;
  }

  /**
   * Determines positions on the given <tt>GM_MultiCurve</tt> where a caption could be drawn. For each of this
   * positons, three candidates are produced; one on the line, one above of it and one below.
   * <p>
   * 
   * @param multiCurve
   * @param element
   * @param g
   * @param projection
   * @return ArrayList containing Arrays of Label-objects
   * @throws FilterEvaluationException
   */
  public static List<Label> createLabels( final GM_MultiCurve multiCurve, final LabelDisplayElement element, final Graphics2D g, final GeoTransform projection ) throws FilterEvaluationException
  {

    final List<Label> placements = Collections.synchronizedList( new ArrayList<Label>() );
    for( int i = 0; i < multiCurve.getSize(); i++ )
    {
      final GM_Curve curve = multiCurve.getCurveAt( i );
      placements.addAll( createLabels( curve, element, g, projection ) );
    }
    return placements;
  }

  /**
   * Determines positions on the given <tt>GM_Curve</tt> where a caption could be drawn. For each of this positons,
   * three candidates are produced; one on the line, one above of it and one below.
   * <p>
   * 
   * @param curve
   * @param element
   * @param g
   * @param projection
   * @return ArrayList containing Arrays of Label-objects
   * @throws FilterEvaluationException
   */
  public static List<Label> createLabels( final GM_Curve curve, final LabelDisplayElement element, final Graphics2D g, final GeoTransform projection ) throws FilterEvaluationException
  {
    final Feature feature = element.getFeature();

    // determine the placement type and parameters from the TextSymbolizer
    double perpendicularOffset = 0.0;
    int placementType = LinePlacement.TYPE_ABSOLUTE;
    double lineWidth = 3.0;
    int gap = 6;
    final TextSymbolizer symbolizer = ((TextSymbolizer) element.getSymbolizer());
    if( symbolizer.getLabelPlacement() != null )
    {
      final LinePlacement linePlacement = symbolizer.getLabelPlacement().getLinePlacement();
      if( linePlacement != null )
      {
        placementType = linePlacement.getPlacementType( element.getFeature() );
        perpendicularOffset = linePlacement.getPerpendicularOffset( element.getFeature() );
        lineWidth = linePlacement.getLineWidth( element.getFeature() );
        gap = linePlacement.getGap( element.getFeature() );
      }
    }

    // get width & height of the caption
    final String caption = element.getLabel().evaluate( element.getFeature() );
    final org.kalypsodeegree.graphics.sld.Font sldFont = symbolizer.getFont();
    final java.awt.Font font = new java.awt.Font( sldFont.getFamily( element.getFeature() ), sldFont.getStyle( element.getFeature() ) | sldFont.getWeight( element.getFeature() ), sldFont.getSize( element.getFeature() ) );
    g.setFont( font );
    final FontRenderContext frc = g.getFontRenderContext();
    final Rectangle2D bounds = font.getStringBounds( caption, frc );
    final LineMetrics metrics = font.getLineMetrics( caption, frc );
    final double width = bounds.getWidth();
    final double height = bounds.getHeight();

    // get screen coordinates of the line
    int[][] pos;
    try
    {
      pos = calcScreenCoordinates( projection, curve );
    }
    catch( GM_Exception e )
    {
      e.printStackTrace();
      return new ArrayList<Label>();
    }

    // ideal distance from the line
    double delta = height / 2.0 + lineWidth / 2.0;

    // walk along the linestring and "collect" possible placement positions
    final int w = (int) width;
    int lastX = pos[0][0];
    int lastY = pos[1][0];
    final int count = pos[2][0];
    int boxStartX = lastX;
    int boxStartY = lastY;

    final List<Label> labels = new ArrayList<Label>( 100 );
    final List<int[]> eCandidates = Collections.synchronizedList( new ArrayList<int[]>( 100 ) );
    int i = 0;
    int kk = 0;
    while( i < count && kk < 100 )
    {
      kk++;
      int x = pos[0][i];
      int y = pos[1][i];

      // segment found where endpoint of box should be located?
      if( getDistance( boxStartX, boxStartY, x, y ) >= w )
      {

        final int[] p0 = new int[] { boxStartX, boxStartY };
        final int[] p1 = new int[] { lastX, lastY };
        final int[] p2 = new int[] { x, y };

        final int[] p = findPointWithDistance( p0, p1, p2, w );
        x = p[0];
        y = p[1];

        lastX = x;
        lastY = y;
        int boxEndX = x;
        int boxEndY = y;

        // does the linesegment run from right to left?
        if( x <= boxStartX )
        {
          boxEndX = boxStartX;
          boxEndY = boxStartY;
          boxStartX = x;
          boxStartY = y;
          x = boxEndX;
          y = boxEndY;
        }

        final double rotation = getRotation( boxStartX, boxStartY, x, y );
        final double[] deviation = calcDeviation( new int[] { boxStartX, boxStartY }, new int[] { boxEndX, boxEndY }, eCandidates );

        Label label = null;

        switch( placementType )
        {
          case LinePlacement.TYPE_ABSOLUTE:
          {
            label = createLabel( caption, font, sldFont.getColor( feature ), metrics, feature, symbolizer.getHalo(), boxStartX, boxStartY, (int) width, (int) height, rotation, 0.0, 0.5, (w - width) / 2, perpendicularOffset );
            break;
          }
          case LinePlacement.TYPE_ABOVE:
          {
            label = createLabel( caption, font, sldFont.getColor( feature ), metrics, feature, symbolizer.getHalo(), boxStartX, boxStartY, (int) width, (int) height, rotation, 0.0, 0.5, (w - width) / 2, delta
                + deviation[0] );
            break;
          }
          case LinePlacement.TYPE_BELOW:
          case LinePlacement.TYPE_AUTO:
          {
            label = createLabel( caption, font, sldFont.getColor( feature ), metrics, feature, symbolizer.getHalo(), boxStartX, boxStartY, (int) width, (int) height, rotation, 0.0, 0.5, (w - width) / 2, -delta
                - deviation[1] );
            break;
          }
          case LinePlacement.TYPE_CENTER:
          {
            label = createLabel( caption, font, sldFont.getColor( feature ), metrics, feature, symbolizer.getHalo(), boxStartX, boxStartY, (int) width, (int) height, rotation, 0.0, 0.5, (w - width) / 2, 0.0 );
            break;
          }
          default:
        }
        labels.add( label );
        boxStartX = lastX;
        boxStartY = lastY;
        eCandidates.clear();
      }
      else
      {
        eCandidates.add( new int[] { x, y } );
        lastX = x;
        lastY = y;
        i++;
      }
    }

    // pick lists of boxes on the linestring
    final List<Label> pick = new ArrayList<Label>( 100 );
    final int n = labels.size();
    for( int j = n / 2; j < labels.size(); j += (gap + 1) )
    {
      pick.add( labels.get( j ) );
    }
    for( int j = n / 2 - (gap + 1); j > 0; j -= (gap + 1) )
    {
      pick.add( labels.get( j ) );
    }
    return pick;
  }

  /**
   * Calculates the maximum deviation that points on a linestring have to the ideal line between the starting point and
   * the end point.
   * <p>
   * The ideal line is thought to be running from left to right, the left deviation value generally is above the line,
   * the right value is below.
   * <p>
   * 
   * @param start
   *            starting point of the linestring
   * @param end
   *            end point of the linestring
   * @param points
   *            points in between
   */
  public static double[] calcDeviation( int[] start, int[] end, final List<int[]> points )
  {

    // extreme deviation to the left
    double d1 = 0.0;
    // extreme deviation to the right
    double d2 = 0.0;
    final Iterator<int[]> it = points.iterator();

    // eventually swap start and end point
    if( start[0] > end[0] )
    {
      final int[] tmp = start;
      start = end;
      end = tmp;
    }

    if( start[0] != end[0] )
    {
      // label orientation is not completly vertical
      if( start[1] != end[1] )
      {
        // label orientation is not completly horizontal
        while( it.hasNext() )
        {
          final int[] point = it.next();
          final double u = ((double) end[1] - (double) start[1]) / ((double) end[0] - (double) start[0]);
          final double x = (u * u * start[0] - u * ((double) start[1] - (double) point[1]) + point[0]) / (1.0 + u * u);
          final double y = (x - start[0]) * u + start[1];
          final double d = getDistance( point, new int[] { (int) (x + 0.5), (int) (y + 0.5) } );
          if( y >= point[1] )
          {
            // candidate for left extreme value
            if( d > d1 )
            {
              d1 = d;
            }
          }
          else if( d > d2 )
          {
            // candidate for right extreme value
            d2 = d;
          }
        }
      }
      else
      {
        // label orientation is completly horizontal
        while( it.hasNext() )
        {
          final int[] point = it.next();
          double d = point[1] - start[1];
          if( d < 0 )
          {
            // candidate for left extreme value
            if( -d > d1 )
            {
              d1 = -d;
            }
          }
          else if( d > d2 )
          {
            // candidate for left extreme value
            d2 = d;
          }
        }
      }
    }
    else
    {
      // label orientation is completly vertical
      while( it.hasNext() )
      {
        final int[] point = it.next();
        double d = point[0] - start[0];
        if( d < 0 )
        {
          // candidate for left extreme value
          if( -d > d1 )
          {
            d1 = -d;
          }
        }
        else if( d > d2 )
        {
          // candidate for right extreme value
          d2 = d;
        }
      }
    }
    return new double[] { d1, d2 };
  }

  /**
   * Finds a point on the line between p1 and p2 that has a certain distance from point p0 (provided that there is such
   * a point).
   * <p>
   * 
   * @param p0
   *            point that is used as reference point for the distance
   * @param p1
   *            starting point of the line
   * @param p2
   *            end point of the line
   * @param d
   *            distance
   */
  public static int[] findPointWithDistance( final int[] p0, final int[] p1, final int[] p2, final int d )
  {

    double x, y;
    final double x0 = p0[0];
    final double y0 = p0[1];
    final double x1 = p1[0];
    final double y1 = p1[1];
    final double x2 = p2[0];
    final double y2 = p2[1];

    if( x1 != x2 )
    {
      // line segment does not run vertical
      final double u = (y2 - y1) / (x2 - x1);
      double p = -2 * (x0 + u * u * x1 - u * (y1 - y0)) / (u * u + 1);
      final double q = ((y1 - y0) * (y1 - y0) + u * u * x1 * x1 + x0 * x0 - 2 * u * x1 * (y1 - y0) - d * d) / (u * u + 1);
      double minX = x1;
      double maxX = x2;
      double minY = y1;
      double maxY = y2;
      if( minX > maxX )
      {
        minX = x2;
        maxX = x1;
      }
      if( minY > maxY )
      {
        minY = y2;
        maxY = y1;
      }
      x = -p / 2 - Math.sqrt( (p / 2) * (p / 2) - q );
      if( x < minX || x > maxX )
      {
        x = -p / 2 + Math.sqrt( (p / 2) * (p / 2) - q );
      }
      y = (x - x1) * u + y1;
    }
    else
    {
      // vertical line segment
      x = x1;
      double minY = y1;
      double maxY = y2;

      if( minY > maxY )
      {
        minY = y2;
        maxY = y1;
      }

      double p = -2 * y0;
      final double q = y0 * y0 + (x1 - x0) * (x1 - x0) - d * d;

      y = -p / 2 - Math.sqrt( (p / 2) * (p / 2) - q );
      if( y < minY || y > maxY )
      {
        y = -p / 2 + Math.sqrt( (p / 2) * (p / 2) - q );
      }
    }
    return new int[] { (int) (x + 0.5), (int) (y + 0.5) };
  }

  public static double getRotation( final double x1, final double y1, final double x2, final double y2 )
  {
    final double dx = x2 - x1;
    final double dy = y2 - y1;
    Debug.debugMethodEnd();
    return Math.atan( dy / dx );
  }

  public static double getDistance( final int[] p1, final int[] p2 )
  {
    final double dx = p1[0] - p2[0];
    final double dy = p1[1] - p2[1];
    return Math.sqrt( dx * dx + dy * dy );
  }

  public static double getDistance( final double x1, final double y1, final double x2, final double y2 )
  {
    final double dx = x2 - x1;
    final double dy = y2 - y1;
    return Math.sqrt( dx * dx + dy * dy );
  }

  /**
   * Calculates the screen coordinates of the given <tt>GM_Curve</tt>.
   */
  public static int[][] calcScreenCoordinates( final GeoTransform projection, final GM_Curve curve ) throws GM_Exception
  {
    final GM_LineString lineString = curve.getAsLineString();
    final int count = lineString.getNumberOfPoints();
    final int[][] pos = new int[3][];
    pos[0] = new int[count];
    pos[1] = new int[count];
    pos[2] = new int[1];

    int k = 0;
    for( int i = 0; i < count; i++ )
    {
      final GM_Position position = lineString.getPositionAt( i );
      final double tx = projection.getDestX( position.getX() );
      final double ty = projection.getDestY( position.getY() );
      if( i > 0 )
      {
        if( getDistance( tx, ty, pos[0][k - 1], pos[1][k - 1] ) > 1 )
        {
          pos[0][k] = (int) (tx + 0.5);
          pos[1][k] = (int) (ty + 0.5);
          k++;
        }
      }
      else
      {
        pos[0][k] = (int) (tx + 0.5);
        pos[1][k] = (int) (ty + 0.5);
        k++;
      }
    }
    pos[2][0] = k;

    Debug.debugMethodEnd();

    return pos;
  }

  /**
   * Returns the physical (screen) coordinates.
   */
  public static int[] calcScreenCoordinates( final GeoTransform projection, final GM_Object geometry )
  {

    final int[] coords = new int[2];

    GM_Position source = null;
    if( geometry instanceof GM_Point )
    {
      source = ((GM_Point) geometry).getPosition();
    }
    else if( geometry instanceof GM_Curve || geometry instanceof GM_MultiCurve )
    {
      source = geometry.getCentroid().getPosition();
    }
    else
    {
      source = geometry.getCentroid().getPosition();
    }

    coords[0] = (int) (projection.getDestX( source.getX() ) + 0.5);
    coords[1] = (int) (projection.getDestY( source.getY() ) + 0.5);
    return coords;
  }
}