/*----------------    FILE HEADER  ------------------------------------------

 This file is part of deegree.
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon Fitzke/Fretter/Poth GbR
 http://www.lat-lon.de

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

 Andreas Poth
 lat/lon Fitzke/Fretter/Poth GbR
 Meckenheimer Allee 176
 53115 Bonn
 Germany
 E-Mail: poth@lat-lon.de

 Jens Fitzke
 Department of Geography
 University of Bonn
 Meckenheimer Allee 166
 53115 Bonn
 Germany
 E-Mail: jens.fitzke@uni-bonn.de

 
 ---------------------------------------------------------------------------*/
package org.deegree_impl.model.geometry;

import java.util.ArrayList;

import org.deegree.model.geometry.ByteUtils;
import org.deegree.model.geometry.GM_Curve;
import org.deegree.model.geometry.GM_CurveSegment;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree.model.geometry.GM_Exception;
import org.deegree.model.geometry.GM_MultiCurve;
import org.deegree.model.geometry.GM_MultiPoint;
import org.deegree.model.geometry.GM_MultiSurface;
import org.deegree.model.geometry.GM_Point;
import org.deegree.model.geometry.GM_Position;
import org.deegree.model.geometry.GM_Surface;
import org.deegree.model.geometry.GM_SurfaceInterpolation;
import org.deegree.model.geometry.GM_SurfacePatch;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * 
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
  public static GM_Envelope createGM_Envelope( double minx, double miny, double maxx, double maxy )
  {
    GM_Position min = createGM_Position( minx, miny );
    GM_Position max = createGM_Position( maxx, maxy );
    return new GM_Envelope_Impl( min, max );
  }

  /**
   * creates a GM_Envelope object out from two croner coordinates
   */
  public static GM_Envelope createGM_Envelope( GM_Position min, GM_Position max )
  {
    return new GM_Envelope_Impl( min, max );
  }

  /**
   * creates a GM_Position from two coordinates.
   */
  public static GM_Position createGM_Position( double x, double y )
  {
    return new GM_Position_Impl( x, y );
  }

  /**
   * creates a GM_Position from two coordinates.
   */
  public static GM_Position createGM_Position( double x, double y, double z )
  {
    return new GM_Position_Impl( new double[]
    { x, y, z } );
  }

  /**
   * creates a GM_Position from an array of double.
   */
  public static GM_Position createGM_Position( double[] p )
  {
    return new GM_Position_Impl( p );
  }

  /**
   * creates a GM_Point from two coordinates.
   */
  public static GM_Point createGM_Point( double x, double y, CS_CoordinateSystem crs )
  {
    return new GM_Point_Impl( x, y, crs );
  }

  /**
   * creates a GM_Point from three coordinates.
   */
  public static GM_Point createGM_Point( double x, double y, double z, CS_CoordinateSystem crs )
  {
    return new GM_Point_Impl( x, y, z, crs );
  }

  /**
   * creates a GM_Point from a position.
   */
  public static GM_Point createGM_Point( GM_Position position, CS_CoordinateSystem crs )
  {
    return new GM_Point_Impl( position, crs );
  }

  /**
   * creates a GM_Point from a wkb.
   */
  public static GM_Point createGM_Point( byte[] wkb, CS_CoordinateSystem srs ) throws GM_Exception
  {
    int wkbType = -1;
    double x = 0;
    double y = 0;

    byte byteorder = wkb[0];

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
   *          array of GM_Point
   * @param crs
   *          spatial reference system of the curve
   */
  public static GM_CurveSegment createGM_CurveSegment( GM_Position[] points, CS_CoordinateSystem crs )
      throws GM_Exception
  {
    return new GM_LineString_Impl( points, crs );
  }

  /**
   * creates a GM_Curve from an array of GM_Positions.
   * 
   * @param positions
   *          positions
   * @param crs
   *          geometries coordinate reference system
   */
  public static GM_Curve createGM_Curve( GM_Position[] positions, CS_CoordinateSystem crs )
      throws GM_Exception
  {
    GM_CurveSegment[] cs = new GM_CurveSegment[1];
    cs[0] = createGM_CurveSegment( positions, crs );
    return new GM_Curve_Impl( cs );
  }

  /**
   * creates a GM_Curve from one curve segment.
   * 
   * @param segment
   *          GM_CurveSegments
   */
  public static GM_Curve createGM_Curve( GM_CurveSegment segment ) throws GM_Exception
  {
    return new GM_Curve_Impl( new GM_CurveSegment[]
    { segment } );
  }

  /**
   * creates a GM_Curve from an array of curve segments.
   * 
   * @param segments
   *          array of GM_CurveSegments
   */
  public static GM_Curve createGM_Curve( GM_CurveSegment[] segments ) throws GM_Exception
  {
    return new GM_Curve_Impl( segments );
  }

  /**
   * creates a GM_Curve from an array of ordinates
   * 
   * @param segments
   *          array of GM_CurveSegments
   */
  public static GM_Curve createGM_Curve( double[] ord, int dim, CS_CoordinateSystem crs )
      throws GM_Exception
  {
    GM_Position[] pos = new GM_Position[ord.length / dim];
    int i = 0;
    int k = 0;
    while( i < ord.length )
    {
      double[] o = new double[dim];
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
   *          exterior ring of the patch
   * @param interiorRings
   *          interior rings of the patch
   * @param si
   *          GM_SurfaceInterpolation
   * @param crs
   *          spatial reference system of the surface patch
   */
  public static GM_SurfacePatch createGM_SurfacePatch( GM_Position[] exteriorRing,
      GM_Position[][] interiorRings, GM_SurfaceInterpolation si, CS_CoordinateSystem crs )
      throws GM_Exception
  {
    return new GM_Polygon_Impl( si, exteriorRing, interiorRings, crs );
  }

  /**
   * creates a GM_Curve from a wkb.
   * 
   * @param wkb
   *          byte stream that contains the wkb information
   * @param crs
   *          spatial reference system of the curve
   */
  public static GM_Curve createGM_Curve( byte[] wkb, CS_CoordinateSystem crs ) throws GM_Exception
  {
    int wkbType = -1;
    int numPoints = -1;
    GM_Position[] points = null;
    double x = 0;
    double y = 0;

    byte byteorder = wkb[0];

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

    GM_CurveSegment[] segment = new GM_CurveSegment[1];

    segment[0] = createGM_CurveSegment( points, crs );

    return createGM_Curve( segment );
  }

  /**
   * creates a GM_Surface composed of one GM_SurfacePatch from array(s) of
   * GM_Position
   * 
   * @param exteriorRing
   *          exterior ring of the patch
   * @param interiorRings
   *          interior rings of the patch
   * @param si
   *          GM_SurfaceInterpolation
   * @param crs
   *          spatial reference system of the surface patch
   */
  public static GM_Surface createGM_Surface( GM_Position[] exteriorRing,
      GM_Position[][] interiorRings, GM_SurfaceInterpolation si, CS_CoordinateSystem crs )
      throws GM_Exception
  {
    GM_SurfacePatch sp = new GM_Polygon_Impl( si, exteriorRing, interiorRings, crs );
    return createGM_Surface( sp );
  }

  /**
   * creates a GM_Surface from an array of GM_SurfacePatch.
   * 
   * @param patch
   *          patches that build the surface
   */
  public static GM_Surface createGM_Surface( GM_SurfacePatch patch ) throws GM_Exception
  {
    return new GM_Surface_Impl( patch );
  }

  /**
   * creates a GM_Surface from a wkb.
   * 
   * @param wkb
   *          byte stream that contains the wkb information
   * @param crs
   *          spatial reference system of the curve
   * @param si
   *          GM_SurfaceInterpolation
   */
  public static GM_Surface createGM_Surface( byte[] wkb, CS_CoordinateSystem crs,
      GM_SurfaceInterpolation si ) throws GM_Exception
  {
    int wkbtype = -1;
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

    GM_SurfacePatch patch = createGM_SurfacePatch( externalBoundary, internalBoundaries, si, crs );

    return createGM_Surface( patch );
  }

  /**
   * Creates a <tt>GM_Surface</tt> from a <tt>GM_Envelope</tt>.
   * <p>
   * 
   * @param bbox
   *          envelope to be converted
   * @param crs
   *          spatial reference system of the surface
   * @return corresponding surface
   * 
   * @throws GM_Exception
   */
  public static GM_Surface createGM_Surface( GM_Envelope bbox, CS_CoordinateSystem crs )
      throws GM_Exception
  {

    GM_Position min = bbox.getMin();
    GM_Position max = bbox.getMax();

    GM_Position[] exteriorRing = new GM_Position[]
    { min, new GM_Position_Impl( max.getX(), min.getY() ), max,
        new GM_Position_Impl( min.getX(), max.getY() ), min };

    return createGM_Surface( exteriorRing, null, new GM_SurfaceInterpolation_Impl(), crs );
  }

  /**
   * Creates a <tt>GM_Surface</tt> from the ordinates of the exterior ring and
   * the the interior rings
   * <p>
   * 
   * @param crs
   *          spatial reference system of the surface
   * @return corresponding surface
   * 
   * @throws GM_Exception
   */
  public static GM_Surface createGM_Surface( double[] exterior, double[][] interior, int dim,
      CS_CoordinateSystem crs ) throws GM_Exception
  {

    // get exterior ring
    GM_Position[] ext = new GM_Position[exterior.length / dim];
    int i = 0;
    int k = 0;
    while( i < exterior.length - 1 )
    {
      double[] o = new double[dim];
      for( int j = 0; j < dim; j++ )
      {
        o[j] = exterior[i++];
      }
      ext[k++] = GeometryFactory.createGM_Position( o );
    }

    // get interior rings if available
    GM_Position[][] in = null;
    if( interior != null && interior.length > 0 )
    {
      in = new GM_Position[interior.length][];
      for( int j = 0; j < in.length; j++ )
      {
        in[j] = new GM_Position[interior[j].length / dim];
        i = 0;
        k = 0;
        while( i < interior[j].length )
        {
          double[] o = new double[dim];
          for( int z = 0; z < dim; z++ )
          {
            o[z] = interior[j][i++];
          }
          in[j][k++] = GeometryFactory.createGM_Position( o );
        }
      }
    }

    // default - linear - interpolation
    GM_SurfaceInterpolation si = new GM_SurfaceInterpolation_Impl();
    return GeometryFactory.createGM_Surface( ext, in, si, crs );
  }

  /**
   * creates a GM_MultiPoint from an array of GM_Point.
   * 
   * @param points
   *          array of GM_Points
   *  
   */
  public static GM_MultiPoint createGM_MultiPoint( GM_Point[] points ) throws GM_Exception
  {
    return new GM_MultiPoint_Impl( points );
  }

  /**
   * creates a GM_MultiPoint from a wkb.
   * 
   * @param wkb
   *          byte stream that contains the wkb information
   * @param crs
   *          spatial reference system of the curve
   *  
   */
  public static GM_MultiPoint createGM_MultiPoint( byte[] wkb, CS_CoordinateSystem crs )
      throws GM_Exception
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

    Object[] o = new Object[3];
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

    return createGM_MultiPoint( points );
  }

  /**
   * creates a GM_MultiCurve from an array of GM_Curves.
   * 
   * @param curves
   */
  public static GM_MultiCurve createGM_MultiCurve( GM_Curve[] curves ) throws GM_Exception
  {
    return new GM_MultiCurve_Impl( curves );
  }

  /**
   * creates a GM_MultiCurve from a wkb.
   * 
   * @param wkb
   *          byte stream that contains the wkb information
   * @param crs
   *          spatial reference system of the curve
   */
  public static GM_MultiCurve createGM_MultiCurve( byte[] wkb, CS_CoordinateSystem crs )
      throws GM_Exception
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
        throw new GM_Exception( "Invalid byte stream for GM_Curve as "
            + " part of a GM_MultiCurve." );
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

    GM_CurveSegment[] segment = new GM_CurveSegment[1];
    GM_Curve[] curves = new GM_Curve[numParts];

    for( int i = 0; i < numParts; i++ )
    {
      segment[0] = createGM_CurveSegment( points[i], crs );
      curves[i] = createGM_Curve( segment );
    }

    return createGM_MultiCurve( curves );
  }

  /**
   * creates a GM_MultiSurface from a wkb
   */
  public static GM_MultiSurface createGM_MultiSurface( GM_Surface[] surfaces ) throws GM_Exception
  {
    return new GM_MultiSurface_Impl( surfaces );
  }

  /**
   * creates a GM_MultiSurface from a wkb
   */
  public static GM_MultiSurface createGM_MultiSurface( byte[] wkb, CS_CoordinateSystem crs,
      GM_SurfaceInterpolation si ) throws GM_Exception
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

    ArrayList list = new ArrayList( numPoly );

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

      GM_SurfacePatch patch = createGM_SurfacePatch( externalBoundary, internalBoundaries, si, crs );

      list.add( createGM_Surface( patch ) );
    }

    GM_MultiSurface multisurface = new GM_MultiSurface_Impl( crs );

    for( int i = 0; i < list.size(); i++ )
    {
      multisurface.addSurface( (GM_Surface)list.get( i ) );
    }

    return multisurface;
  }

}