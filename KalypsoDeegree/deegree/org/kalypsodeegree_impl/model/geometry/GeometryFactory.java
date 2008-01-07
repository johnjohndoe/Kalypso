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
package org.kalypsodeegree_impl.model.geometry;

import java.awt.Point;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.ByteUtils;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_CurveSegment;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_MultiCurve;
import org.kalypsodeegree.model.geometry.GM_MultiPoint;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Ring;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfaceInterpolation;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;
import org.kalypsodeegree.model.geometry.GM_Triangle;
import org.kalypsodeegree.model.geometry.GM_TriangulatedSurface;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * <p>
 * ------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
final public class GeometryFactory
{
  /**
   * creates a GM_Envelope object out from two croner coordinates
   */
  public static GM_Envelope createGM_Envelope( final double minx, final double miny, final double maxx, final double maxy )
  {
    final GM_Position min = GeometryFactory.createGM_Position( minx < maxx ? minx : maxx, miny < maxy ? miny : maxy );
    final GM_Position max = GeometryFactory.createGM_Position( minx > maxx ? minx : maxx, miny > maxy ? miny : maxy );
    return new GM_Envelope_Impl( min, max );
  }

  /**
   * creates a GM_Envelope object out from two croner coordinates
   */
  public static GM_Envelope createGM_Envelope( final GM_Position min, final GM_Position max )
  {
    return new GM_Envelope_Impl( min, max );
  }

  /**
   * creates a GM_Position from two coordinates.
   */
  public static GM_Position createGM_Position( final double x, final double y )
  {
    return new GM_Position_Impl( x, y );
  }

  /**
   * creates a GM_Position from three coordinates.
   */
  public static GM_Position createGM_Position( final double x, final double y, final double z )
  {
    return new GM_Position_Impl( new double[] { x, y, z } );
  }

  /**
   * creates a GM_Position from an array of double.
   */
  public static GM_Position createGM_Position( final double[] p )
  {
    return new GM_Position_Impl( p );
  }

  /**
   * creates a GM_Point from two coordinates.
   */
  public static GM_Point createGM_Point( final double x, final double y, final CS_CoordinateSystem crs )
  {
    return new GM_Point_Impl( x, y, crs );
  }

  /**
   * creates a GM_Point from three coordinates.
   */
  public static GM_Point createGM_Point( final double x, final double y, final double z, final CS_CoordinateSystem crs )
  {
    return new GM_Point_Impl( x, y, z, crs );
  }

  /**
   * creates a GM_Point from a position.
   */
  public static GM_Point createGM_Point( final GM_Position position, final CS_CoordinateSystem crs )
  {
    return new GM_Point_Impl( position, crs );
  }

  /**
   * creates a GM_Point from a wkb.
   */
  public static GM_Point createGM_Point( final byte[] wkb, final CS_CoordinateSystem srs ) throws GM_Exception
  {
    int wkbType = -1;
    double x = 0;
    double y = 0;

    final byte byteorder = wkb[0];

    if( byteorder == 0 )
    {
      wkbType = ByteUtils.readBEInt( wkb, 1 );
    }
    else
    {
      wkbType = ByteUtils.readLEInt( wkb, 1 );
    }

    if( wkbType != 1 )
    {
      throw new GM_Exception( "invalid byte stream" );
    }

    if( byteorder == 0 )
    {
      x = ByteUtils.readBEDouble( wkb, 5 );
      y = ByteUtils.readBEDouble( wkb, 13 );
    }
    else
    {
      x = ByteUtils.readLEDouble( wkb, 5 );
      y = ByteUtils.readLEDouble( wkb, 13 );
    }

    return new GM_Point_Impl( x, y, srs );
  }

  /**
   * creates a GM_CurveSegment from an array of points.
   * 
   * @param points
   *            array of GM_Point
   * @param crs
   *            spatial reference system of the curve
   */
  public static GM_CurveSegment createGM_CurveSegment( final GM_Position[] points, final CS_CoordinateSystem crs ) throws GM_Exception
  {
    return new GM_LineString_Impl( points, crs );
  }

  /**
   * creates a GM_Curve from an array of GM_Positions.
   * 
   * @param positions
   *            positions
   * @param crs
   *            geometries coordinate reference system
   */
  public static GM_Curve createGM_Curve( final GM_Position[] positions, final CS_CoordinateSystem crs ) throws GM_Exception
  {
    final GM_CurveSegment[] cs = new GM_CurveSegment[1];
    cs[0] = GeometryFactory.createGM_CurveSegment( positions, crs );
    return new GM_Curve_Impl( cs );
  }

  /**
   * creates a GM_Curve from one curve segment.
   * 
   * @param segment
   *            GM_CurveSegments
   */
  public static GM_Curve createGM_Curve( final GM_CurveSegment segment ) throws GM_Exception
  {
    return new GM_Curve_Impl( new GM_CurveSegment[] { segment } );
  }

  /**
   * creates a GM_Curve from an array of curve segments.
   * 
   * @param segments
   *            array of GM_CurveSegments
   */
  public static GM_Curve createGM_Curve( final GM_CurveSegment[] segments ) throws GM_Exception
  {
    return new GM_Curve_Impl( segments );
  }

  /**
   * creates a GM_Curve from an array of ordinates
   */
  public static GM_Curve createGM_Curve( final double[] ord, final int dim, final CS_CoordinateSystem crs ) throws GM_Exception
  {
    final GM_Position[] pos = new GM_Position[ord.length / dim];
    int i = 0;
    int k = 0;
    while( i < ord.length )
    {
      final double[] o = new double[dim];
      for( int j = 0; j < dim; j++ )
      {
        o[j] = ord[i++];
      }
      pos[k++] = GeometryFactory.createGM_Position( o );
    }
    return GeometryFactory.createGM_Curve( pos, crs );
  }

  /**
   * creates a GM_SurfacePatch from array(s) of GM_Position
   * 
   * @param exteriorRing
   *            exterior ring of the patch
   * @param interiorRings
   *            interior rings of the patch
   * @param si
   *            GM_SurfaceInterpolation
   * @param crs
   *            spatial reference system of the surface patch
   */
  public static GM_SurfacePatch createGM_SurfacePatch( final GM_Position[] exteriorRing, final GM_Position[][] interiorRings, final GM_SurfaceInterpolation si, final CS_CoordinateSystem crs ) throws GM_Exception
  {
    return new GM_Polygon_Impl( si, exteriorRing, interiorRings, crs );
  }

  public static GM_SurfacePatch createGM_SurfacePatch( final double[] exterior, final double[][] interior, final int dim, final CS_CoordinateSystem crs ) throws GM_Exception
  {
    final GM_Position[] ext = positionsFromDoubles( exterior, dim );
    final GM_Position[][] in;
    if( interior == null || interior.length == 0 )
      in = null;
    else
    {
      in = new GM_Position[interior.length][];
      for( int j = 0; j < in.length; j++ )
        in[j] = positionsFromDoubles( interior[j], dim );
    }

    return createGM_SurfacePatch( ext, in, new GM_SurfaceInterpolation_Impl(), crs );
  }

  public static GM_SurfacePatch createGM_SurfacePatch( final GM_Ring exterior, final GM_Ring[] interior, final CS_CoordinateSystem crs ) throws GM_Exception
  {
    final GM_Position[] ext = exterior.getPositions();
    final GM_Position[][] in;
    if( interior == null || interior.length == 0 )
      in = null;
    else
    {
      in = new GM_Position[interior.length][];
      for( int j = 0; j < in.length; j++ )
        in[j] = interior[j].getPositions();
    }

    return createGM_SurfacePatch( ext, in, new GM_SurfaceInterpolation_Impl(), crs );
  }

  /**
   * creates a GM_Curve from a wkb.
   * 
   * @param wkb
   *            byte stream that contains the wkb information
   * @param crs
   *            spatial reference system of the curve
   */
  public static GM_Curve createGM_Curve( final byte[] wkb, final CS_CoordinateSystem crs ) throws GM_Exception
  {
    int wkbType = -1;
    int numPoints = -1;
    GM_Position[] points = null;
    double x = 0;
    double y = 0;

    final byte byteorder = wkb[0];

    if( byteorder == 0 )
    {
      wkbType = ByteUtils.readBEInt( wkb, 1 );
    }
    else
    {
      wkbType = ByteUtils.readLEInt( wkb, 1 );
    }

    // check if it's realy a linestrin/curve
    if( wkbType != 2 )
    {
      throw new GM_Exception( "invalid byte stream for GM_Curve" );
    }

    // read number of points
    if( byteorder == 0 )
    {
      numPoints = ByteUtils.readBEInt( wkb, 5 );
    }
    else
    {
      numPoints = ByteUtils.readLEInt( wkb, 5 );
    }

    int offset = 9;

    points = new GM_Position[numPoints];

    // read the i-th point depending on the byteorde
    if( byteorder == 0 )
    {
      for( int i = 0; i < numPoints; i++ )
      {
        x = ByteUtils.readBEDouble( wkb, offset );
        offset += 8;
        y = ByteUtils.readBEDouble( wkb, offset );
        offset += 8;
        points[i] = new GM_Position_Impl( x, y );
      }
    }
    else
    {
      for( int i = 0; i < numPoints; i++ )
      {
        x = ByteUtils.readLEDouble( wkb, offset );
        offset += 8;
        y = ByteUtils.readLEDouble( wkb, offset );
        offset += 8;
        points[i] = new GM_Position_Impl( x, y );
      }
    }

    final GM_CurveSegment[] segment = new GM_CurveSegment[1];

    segment[0] = GeometryFactory.createGM_CurveSegment( points, crs );

    return GeometryFactory.createGM_Curve( segment );
  }

  /**
   * creates a GM_Surface composed of one GM_SurfacePatch from array(s) of GM_Position
   * 
   * @param exteriorRing
   *            exterior ring of the patch
   * @param interiorRings
   *            interior rings of the patch
   * @param si
   *            GM_SurfaceInterpolation
   * @param crs
   *            spatial reference system of the surface patch
   */
  public static GM_Surface<GM_SurfacePatch> createGM_Surface( final GM_Position[] exteriorRing, final GM_Position[][] interiorRings, final GM_SurfaceInterpolation si, final CS_CoordinateSystem crs ) throws GM_Exception
  {
    final GM_SurfacePatch sp = new GM_Polygon_Impl( si, exteriorRing, interiorRings, crs );
    return GeometryFactory.createGM_Surface( sp );
  }

  /**
   * creates a GM_Surface from an array of GM_SurfacePatch.
   * 
   * @param patch
   *            patches that build the surface
   */
  public static GM_Surface<GM_SurfacePatch> createGM_Surface( final GM_SurfacePatch patch ) throws GM_Exception
  {
    return new GM_Surface_Impl<GM_SurfacePatch>( patch );
  }

  /**
   * creates a GM_Surface from a wkb.
   * 
   * @param wkb
   *            byte stream that contains the wkb information
   * @param crs
   *            spatial reference system of the curve
   * @param si
   *            GM_SurfaceInterpolation
   */
  public static GM_Surface<GM_SurfacePatch> createGM_Surface( final byte[] wkb, final CS_CoordinateSystem crs, final GM_SurfaceInterpolation si ) throws GM_Exception
  {
    int wkbtype = -1;
    int numRings = 0;
    int numPoints = 0;
    int offset = 0;
    double x = 0;
    double y = 0;

    GM_Position[] externalBoundary = null;
    GM_Position[][] internalBoundaries = null;

    final byte byteorder = wkb[offset++];

    if( byteorder == 0 )
    {
      wkbtype = ByteUtils.readBEInt( wkb, offset );
    }
    else
    {
      wkbtype = ByteUtils.readLEInt( wkb, offset );
    }

    offset += 4;

    if( wkbtype == 6 )
    {
      return null;
    }

    // is the geometry respresented by wkb a polygon?
    if( wkbtype != 3 )
    {
      throw new GM_Exception( "invalid byte stream for GM_Surface " + wkbtype );
    }

    // read number of rings of the polygon
    if( byteorder == 0 )
    {
      numRings = ByteUtils.readBEInt( wkb, offset );
    }
    else
    {
      numRings = ByteUtils.readLEInt( wkb, offset );
    }

    offset += 4;

    // read number of points of the external ring
    if( byteorder == 0 )
    {
      numPoints = ByteUtils.readBEInt( wkb, offset );
    }
    else
    {
      numPoints = ByteUtils.readLEInt( wkb, offset );
    }

    offset += 4;

    // allocate memory for the external boundary
    externalBoundary = new GM_Position[numPoints];

    if( byteorder == 0 )
    {
      // read points of the external boundary from the byte[]
      for( int i = 0; i < numPoints; i++ )
      {
        x = ByteUtils.readBEDouble( wkb, offset );
        offset += 8;
        y = ByteUtils.readBEDouble( wkb, offset );
        offset += 8;
        externalBoundary[i] = new GM_Position_Impl( x, y );
      }
    }
    else
    {
      // read points of the external boundary from the byte[]
      for( int i = 0; i < numPoints; i++ )
      {
        x = ByteUtils.readLEDouble( wkb, offset );
        offset += 8;
        y = ByteUtils.readLEDouble( wkb, offset );
        offset += 8;
        externalBoundary[i] = new GM_Position_Impl( x, y );
      }
    }

    // only if numRings is larger then one there internal rings
    if( numRings > 1 )
    {
      internalBoundaries = new GM_Position[numRings - 1][];
    }

    if( byteorder == 0 )
    {
      for( int j = 1; j < numRings; j++ )
      {
        // read number of points of the j-th internal ring
        numPoints = ByteUtils.readBEInt( wkb, offset );
        offset += 4;

        // allocate memory for the j-th internal boundary
        internalBoundaries[j - 1] = new GM_Position[numPoints];

        // read points of the external boundary from the byte[]
        for( int i = 0; i < numPoints; i++ )
        {
          x = ByteUtils.readBEDouble( wkb, offset );
          offset += 8;
          y = ByteUtils.readBEDouble( wkb, offset );
          offset += 8;
          internalBoundaries[j - 1][i] = new GM_Position_Impl( x, y );
        }
      }
    }
    else
    {
      for( int j = 1; j < numRings; j++ )
      {
        // read number of points of the j-th internal ring
        numPoints = ByteUtils.readLEInt( wkb, offset );
        offset += 4;

        // allocate memory for the j-th internal boundary
        internalBoundaries[j - 1] = new GM_Position[numPoints];

        // read points of the external boundary from the byte[]
        for( int i = 0; i < numPoints; i++ )
        {
          x = ByteUtils.readLEDouble( wkb, offset );
          offset += 8;
          y = ByteUtils.readLEDouble( wkb, offset );
          offset += 8;
          internalBoundaries[j - 1][i] = new GM_Position_Impl( x, y );
        }
      }
    }

    final GM_SurfacePatch patch = GeometryFactory.createGM_SurfacePatch( externalBoundary, internalBoundaries, si, crs );

    return GeometryFactory.createGM_Surface( patch );
  }

  /**
   * Creates a <tt>GM_Surface</tt> from a <tt>GM_Envelope</tt>.
   * <p>
   * 
   * @param bbox
   *            envelope to be converted
   * @param crs
   *            spatial reference system of the surface
   * @return corresponding surface
   * @throws GM_Exception
   */
  public static GM_Surface<GM_SurfacePatch> createGM_Surface( final GM_Envelope bbox, final CS_CoordinateSystem crs ) throws GM_Exception
  {

    final GM_Position min = bbox.getMin();
    final GM_Position max = bbox.getMax();

    final GM_Position[] exteriorRing = new GM_Position[] { min, new GM_Position_Impl( max.getX(), min.getY() ), max, new GM_Position_Impl( min.getX(), max.getY() ), min };

    return GeometryFactory.createGM_Surface( exteriorRing, null, new GM_SurfaceInterpolation_Impl(), crs );
  }

  /**
   * Creates a <tt>GM_Surface</tt> from the ordinates of the exterior ring and the the interior rings
   * <p>
   * 
   * @param crs
   *            spatial reference system of the surface
   * @return corresponding surface
   * @throws GM_Exception
   */
  public static GM_Surface<GM_SurfacePatch> createGM_Surface( final double[] exterior, final double[][] interior, final int dim, final CS_CoordinateSystem crs ) throws GM_Exception
  {
    // get exterior ring
    final GM_Position[] ext = positionsFromDoubles( exterior, dim );

    // get interior rings if available
    final GM_Position[][] in;
    if( interior == null || interior.length == 0 )
      in = null;
    else
    {
      in = new GM_Position[interior.length][];
      for( int j = 0; j < in.length; j++ )
        in[j] = positionsFromDoubles( interior[j], dim );
    }

    // default - linear - interpolation
    final GM_SurfaceInterpolation si = new GM_SurfaceInterpolation_Impl();
    return GeometryFactory.createGM_Surface( ext, in, si, crs );
  }

  private static GM_Position[] positionsFromDoubles( final double[] exterior, final int dim )
  {
    final GM_Position[] poses = new GM_Position[exterior.length / dim];
    for( int i = 0; i < poses.length; i++ )
    {
      final double[] o = new double[dim];
      for( int j = 0; j < dim; j++ )
        o[j] = exterior[i * dim + j];

      poses[i] = GeometryFactory.createGM_Position( o );
    }

    return poses;
  }

  /**
   * creates a GM_MultiPoint from an array of GM_Point.
   * 
   * @param points
   *            array of GM_Points
   */
  public static GM_MultiPoint createGM_MultiPoint( final GM_Point[] points )
  {
    return new GM_MultiPoint_Impl( points );
  }

  public static GM_MultiPoint createGM_MultiPoint( final GM_Point[] points, final CS_CoordinateSystem crs )
  {
    return new GM_MultiPoint_Impl( points, crs );
  }

  /**
   * creates a GM_MultiPoint from a wkb.
   * 
   * @param wkb
   *            byte stream that contains the wkb information
   * @param crs
   *            spatial reference system of the curve
   */
  public static GM_MultiPoint createGM_MultiPoint( final byte[] wkb, final CS_CoordinateSystem crs ) throws GM_Exception
  {
    GM_Point[] points = null;
    int wkbType = -1;
    int numPoints = -1;
    double x = 0;
    double y = 0;

    byte byteorder = wkb[0];

    // read wkbType
    if( byteorder == 0 )
    {
      wkbType = ByteUtils.readBEInt( wkb, 1 );
    }
    else
    {
      wkbType = ByteUtils.readLEInt( wkb, 1 );
    }

    // if the geometry isn't a multipoint throw exception
    if( wkbType != 4 )
    {
      throw new GM_Exception( "Invalid byte stream for GM_MultiPoint" );
    }

    // read number of points
    if( byteorder == 0 )
    {
      numPoints = ByteUtils.readBEInt( wkb, 5 );
    }
    else
    {
      numPoints = ByteUtils.readLEInt( wkb, 5 );
    }

    points = new GM_Point[numPoints];

    int offset = 9;

    final Object[] o = new Object[3];
    o[2] = crs;

    // read all points
    for( int i = 0; i < numPoints; i++ )
    {
      // byteorder of the i-th point
      byteorder = wkb[offset];

      // wkbType of the i-th geometry
      if( byteorder == 0 )
      {
        wkbType = ByteUtils.readBEInt( wkb, offset + 1 );
      }
      else
      {
        wkbType = ByteUtils.readLEInt( wkb, offset + 1 );
      }

      // if the geometry isn't a point throw exception
      if( wkbType != 1 )
      {
        throw new GM_Exception( "Invalid byte stream for GM_Point as " + "part of a multi point" );
      }

      // read the i-th point depending on the byteorde
      if( byteorder == 0 )
      {
        x = ByteUtils.readBEDouble( wkb, offset + 5 );
        y = ByteUtils.readBEDouble( wkb, offset + 13 );
      }
      else
      {
        x = ByteUtils.readLEDouble( wkb, offset + 5 );
        y = ByteUtils.readLEDouble( wkb, offset + 13 );
      }

      offset += 21;

      points[i] = new GM_Point_Impl( x, y, crs );
    }

    return GeometryFactory.createGM_MultiPoint( points );
  }

  /**
   * creates a GM_MultiCurve from an array of GM_Curves.
   * 
   * @param curves
   */
  public static GM_MultiCurve createGM_MultiCurve( final GM_Curve[] curves )
  {
    return new GM_MultiCurve_Impl( curves );
  }

  /**
   * creates a GM_MultiCurve from a wkb.
   * 
   * @param wkb
   *            byte stream that contains the wkb information
   * @param crs
   *            spatial reference system of the curve
   */
  public static GM_MultiCurve createGM_MultiCurve( final byte[] wkb, final CS_CoordinateSystem crs ) throws GM_Exception
  {
    int wkbType = -1;
    int numPoints = -1;
    int numParts = -1;
    double x = 0;
    double y = 0;
    GM_Position[][] points = null;

    int offset = 0;

    byte byteorder = wkb[offset++];

    if( byteorder == 0 )
    {
      wkbType = ByteUtils.readBEInt( wkb, offset );
    }
    else
    {
      wkbType = ByteUtils.readLEInt( wkb, offset );
    }

    offset += 4;

    // check if it's realy a linestring
    if( wkbType != 5 )
    {
      throw new GM_Exception( "Invalid byte stream for GM_MultiCurve" );
    }

    // read number of linestrings
    if( byteorder == 0 )
    {
      numParts = ByteUtils.readBEInt( wkb, offset );
    }
    else
    {
      numParts = ByteUtils.readLEInt( wkb, offset );
    }

    offset += 4;

    points = new GM_Position[numParts][];

    // for every linestring
    for( int j = 0; j < numParts; j++ )
    {
      byteorder = wkb[offset++];

      if( byteorder == 0 )
      {
        wkbType = ByteUtils.readBEInt( wkb, offset );
      }
      else
      {
        wkbType = ByteUtils.readLEInt( wkb, offset );
      }

      offset += 4;

      // check if it's realy a linestring
      if( wkbType != 2 )
      {
        throw new GM_Exception( "Invalid byte stream for GM_Curve as " + " part of a GM_MultiCurve." );
      }

      // read number of points
      if( byteorder == 0 )
      {
        numPoints = ByteUtils.readBEInt( wkb, offset );
      }
      else
      {
        numPoints = ByteUtils.readLEInt( wkb, offset );
      }

      offset += 4;

      points[j] = new GM_Position[numPoints];

      // read the i-th point depending on the byteorde
      if( byteorder == 0 )
      {
        for( int i = 0; i < numPoints; i++ )
        {
          x = ByteUtils.readBEDouble( wkb, offset );
          offset += 8;
          y = ByteUtils.readBEDouble( wkb, offset );
          offset += 8;
          points[j][i] = new GM_Position_Impl( x, y );
        }
      }
      else
      {
        for( int i = 0; i < numPoints; i++ )
        {
          x = ByteUtils.readLEDouble( wkb, offset );
          offset += 8;
          y = ByteUtils.readLEDouble( wkb, offset );
          offset += 8;
          points[j][i] = new GM_Position_Impl( x, y );
        }
      }
    }

    final GM_CurveSegment[] segment = new GM_CurveSegment[1];
    final GM_Curve[] curves = new GM_Curve[numParts];

    for( int i = 0; i < numParts; i++ )
    {
      segment[0] = GeometryFactory.createGM_CurveSegment( points[i], crs );
      curves[i] = GeometryFactory.createGM_Curve( segment );
    }

    return GeometryFactory.createGM_MultiCurve( curves );
  }

  /**
   * creates a GM_MultiSurface
   */
  public static GM_MultiSurface createGM_MultiSurface( final GM_Surface<GM_SurfacePatch>[] surfaces, final CS_CoordinateSystem crs )
  {
    return new GM_MultiSurface_Impl( surfaces, crs );
  }

  /**
   * creates a GM_MultiSurface from a wkb
   */
  public static GM_MultiSurface createGM_MultiSurface( final byte[] wkb, final CS_CoordinateSystem crs, final GM_SurfaceInterpolation si ) throws GM_Exception
  {
    int wkbtype = -1;
    int numPoly = 0;
    int numRings = 0;
    int numPoints = 0;
    int offset = 0;
    double x = 0;
    double y = 0;
    GM_Position[] externalBoundary = null;
    GM_Position[][] internalBoundaries = null;

    byte byteorder = wkb[offset++];

    if( byteorder == 0 )
    {
      wkbtype = ByteUtils.readBEInt( wkb, offset );
    }
    else
    {
      wkbtype = ByteUtils.readLEInt( wkb, offset );
    }

    offset += 4;

    // is the wkbmetry a multipolygon?
    if( wkbtype != 6 )
    {
      throw new GM_Exception( "Invalid byte stream for GM_MultiSurface" );
    }

    // read number of polygons on the byte[]
    if( byteorder == 0 )
    {
      numPoly = ByteUtils.readBEInt( wkb, offset );
    }
    else
    {
      numPoly = ByteUtils.readLEInt( wkb, offset );
    }

    offset += 4;

    final List<GM_Surface<GM_SurfacePatch>> list = new ArrayList<GM_Surface<GM_SurfacePatch>>( numPoly );

    for( int ip = 0; ip < numPoly; ip++ )
    {
      byteorder = wkb[offset];
      offset++;

      if( byteorder == 0 )
      {
        wkbtype = ByteUtils.readBEInt( wkb, offset );
      }
      else
      {
        wkbtype = ByteUtils.readLEInt( wkb, offset );
      }

      offset += 4;

      // is the geometry respresented by wkb a polygon?
      if( wkbtype != 3 )
      {
        throw new GM_Exception( "invalid byte stream for GM_Surface " + wkbtype );
      }

      // read number of rings of the polygon
      if( byteorder == 0 )
      {
        numRings = ByteUtils.readBEInt( wkb, offset );
      }
      else
      {
        numRings = ByteUtils.readLEInt( wkb, offset );
      }

      offset += 4;

      // read number of points of the external ring
      if( byteorder == 0 )
      {
        numPoints = ByteUtils.readBEInt( wkb, offset );
      }
      else
      {
        numPoints = ByteUtils.readLEInt( wkb, offset );
      }

      offset += 4;

      // allocate memory for the external boundary
      externalBoundary = new GM_Position[numPoints];

      if( byteorder == 0 )
      {
        // read points of the external boundary from the byte[]
        for( int i = 0; i < numPoints; i++ )
        {
          x = ByteUtils.readBEDouble( wkb, offset );
          offset += 8;
          y = ByteUtils.readBEDouble( wkb, offset );
          offset += 8;
          externalBoundary[i] = new GM_Position_Impl( x, y );
        }
      }
      else
      {
        // read points of the external boundary from the byte[]
        for( int i = 0; i < numPoints; i++ )
        {
          x = ByteUtils.readLEDouble( wkb, offset );
          offset += 8;
          y = ByteUtils.readLEDouble( wkb, offset );
          offset += 8;
          externalBoundary[i] = new GM_Position_Impl( x, y );
        }
      }

      // only if numRings is larger then one there internal rings
      if( numRings > 1 )
      {
        internalBoundaries = new GM_Position[numRings - 1][];
      }

      if( byteorder == 0 )
      {
        for( int j = 1; j < numRings; j++ )
        {
          // read number of points of the j-th internal ring
          numPoints = ByteUtils.readBEInt( wkb, offset );
          offset += 4;

          // allocate memory for the j-th internal boundary
          internalBoundaries[j - 1] = new GM_Position[numPoints];

          // read points of the external boundary from the byte[]
          for( int i = 0; i < numPoints; i++ )
          {
            x = ByteUtils.readBEDouble( wkb, offset );
            offset += 8;
            y = ByteUtils.readBEDouble( wkb, offset );
            offset += 8;
            internalBoundaries[j - 1][i] = new GM_Position_Impl( x, y );
          }
        }
      }
      else
      {
        for( int j = 1; j < numRings; j++ )
        {
          // read number of points of the j-th internal ring
          numPoints = ByteUtils.readLEInt( wkb, offset );
          offset += 4;

          // allocate memory for the j-th internal boundary
          internalBoundaries[j - 1] = new GM_Position[numPoints];

          // read points of the external boundary from the byte[]
          for( int i = 0; i < numPoints; i++ )
          {
            x = ByteUtils.readLEDouble( wkb, offset );
            offset += 8;
            y = ByteUtils.readLEDouble( wkb, offset );
            offset += 8;
            internalBoundaries[j - 1][i] = new GM_Position_Impl( x, y );
          }
        }
      }

      final GM_SurfacePatch patch = GeometryFactory.createGM_SurfacePatch( externalBoundary, internalBoundaries, si, crs );

      list.add( GeometryFactory.createGM_Surface( patch ) );
    }

    final GM_MultiSurface multisurface = new GM_MultiSurface_Impl( crs );

    for( int i = 0; i < list.size(); i++ )
    {
      multisurface.addSurface( list.get( i ) );
    }

    return multisurface;
  }

  public static GM_Point createGM_Point( final Point p, final GeoTransform transform, final CS_CoordinateSystem coordinatesSystem )
  {
    final double g1x = transform.getSourceX( p.getX() );
    final double g1y = transform.getSourceY( p.getY() );
    return GeometryFactory.createGM_Point( g1x, g1y, coordinatesSystem );
  }

  public static GM_Position[] cloneGM_Position( final GM_Position[] positions )
  {
    final List<GM_Position> myList = new LinkedList<GM_Position>();

    for( final GM_Position position : positions )
    {
      myList.add( GeometryFactory.createGM_Position( position.getAsArray() ) );
    }

    return myList.toArray( new GM_Position[] {} );
  }

  public static GM_Triangle_Impl createGM_Triangle( final GM_Position[] pos, final CS_CoordinateSystem crs ) throws GM_Exception
  {
    if( pos.length != 3 )
      return null;

    return new GM_Triangle_Impl( pos[0], pos[1], pos[2], crs );
  }

  public static GM_Triangle_Impl createGM_Triangle( final GM_Position pos1, final GM_Position pos2, final GM_Position pos3, final CS_CoordinateSystem crs ) throws GM_Exception
  {
    return new GM_Triangle_Impl( pos1, pos2, pos3, crs );
  }

  public static GM_TriangulatedSurface createGM_TriangulatedSurface( final CS_CoordinateSystem crs ) throws GM_Exception
  {
    return new GM_TriangulatedSurface_Impl( crs );
  }

  /**
   * creates a GM_Curve from an double array of GM_Positions.
   * 
   * @param positions
   *            positions
   * @param crs
   *            geometries coordinate reference system
   */
  public static GM_Curve[] createGM_Curve( final GM_Position[][] rings, final CS_CoordinateSystem crs ) throws GM_Exception
  {
    final List<GM_Curve> curveList = new LinkedList<GM_Curve>();

    for( final GM_Position[] positions : rings )
    {
      final GM_CurveSegment[] cs = new GM_CurveSegment[1];
      cs[0] = GeometryFactory.createGM_CurveSegment( positions, crs );

      curveList.add( new GM_Curve_Impl( cs ) );
    }
    return curveList.toArray( new GM_Curve[curveList.size()] );
  }

  public static GM_Ring[] createGM_Rings( final GM_Position[][] rings, final CS_CoordinateSystem crs ) throws GM_Exception
  {
    final List<GM_Ring> ringList = new LinkedList<GM_Ring>();

    for( final GM_Position[] positions : rings )
      ringList.add( createGM_Ring( positions, crs ) );

    return ringList.toArray( new GM_Ring[ringList.size()] );
  }

  public static GM_Ring_Impl createGM_Ring( final GM_Position[] positions, final CS_CoordinateSystem crs ) throws GM_Exception
  {
    return new GM_Ring_Impl( positions, crs );
  }

  public static GM_TriangulatedSurface createGM_TriangulatedSurface( GM_Triangle[] triangles, CS_CoordinateSystem targetOGCCS ) throws GM_Exception
  {
    GM_TriangulatedSurface triangulatedSurface = createGM_TriangulatedSurface( targetOGCCS );

    for( GM_Triangle triangle : triangles )
    {
      triangulatedSurface.add( triangle );
    }
    return triangulatedSurface;
  }
}