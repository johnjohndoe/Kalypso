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
package org.kalypsodeegree_impl.model.geometry;

import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_LineString;
import org.kalypsodeegree.model.geometry.GM_MultiCurve;
import org.kalypsodeegree.model.geometry.GM_MultiPoint;
import org.kalypsodeegree.model.geometry.GM_MultiPrimitive;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Polygon;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryCollection;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.MultiLineString;
import com.vividsolutions.jts.geom.MultiPoint;
import com.vividsolutions.jts.geom.MultiPolygon;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.geom.PrecisionModel;

/**
 * Adapter between deegree- <tt>GM_Object</tt> s and JTS- <tt>Geometry<tt> objects.
 * <p>
 * Please note that the generated deegree-objects use null as
 * <tt>CS_CoordinateSystem</tt>!
 * <p>
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider</a>
 * @version $Revision$ $Date$
 */
public class JTSAdapter
{
  // precision model that is used for all JTS-Geometries
  public static PrecisionModel pm = new PrecisionModel();

  // factory for creating JTS-Geometries
  public static com.vividsolutions.jts.geom.GeometryFactory jtsFactory = new com.vividsolutions.jts.geom.GeometryFactory( pm, 0 );

  /**
   * Converts a <tt>GM_Object</tt> to a corresponding JTS- <tt>Geometry</tt> object.
   * <p>
   * Currently, the following conversions are supported:
   * <ul>
   * <li>GM_Point -> Point
   * <li>GM_MultiPoint -> MultiPoint
   * <li>GM_Curve -> LineString
   * <li>GM_MultiCurve -> MultiLineString
   * <li>GM_Surface -> Polygon
   * <li>GM_MultiSurface -> MultiPolygon
   * <li>GM_MultiPrimitive -> GeometryCollection
   * </ul>
   * <p>
   * 
   * @param gmObject
   *            the object to be converted
   * @return the corresponding JTS- <tt>Geometry</tt> object
   * @throws GM_Exception
   *             if type unsupported or conversion failed
   */
  public static Geometry export( final GM_Object gmObject ) throws GM_Exception
  {
    if( gmObject == null )
      return null;

    if( gmObject instanceof GM_Point )
      return export( (GM_Point) gmObject );

    if( gmObject instanceof GM_MultiPoint )
      return export( (GM_MultiPoint) gmObject );

    if( gmObject instanceof GM_Curve )
      return export( (GM_Curve) gmObject );

    if( gmObject instanceof GM_MultiCurve )
      return export( (GM_MultiCurve) gmObject );

    if( gmObject instanceof GM_Surface )
      return export( (GM_Surface< ? >) gmObject );

    if( gmObject instanceof GM_MultiSurface )
      return export( (GM_MultiSurface) gmObject );

    if( gmObject instanceof GM_MultiPrimitive )
      return export( (GM_MultiPrimitive) gmObject );

    throw new GM_Exception( "JTSAdapter.export does not support type '" + gmObject.getClass().getName() + "'!" );
  }

  /**
   * Converts a JTS- <tt>Geometry</tt> object to a corresponding <tt>GM_Object</tt>.
   * <p>
   * Currently, the following conversions are supported:
   * <ul>
   * <li>Point -> GM_Point
   * <li>MultiPoint -> GM_MultiPoint
   * <li>LineString -> GM_Curve
   * <li>MultiLineString -> GM_MultiCurve
   * <li>Polygon -> GM_Surface
   * <li>MultiPolygon -> GM_MultiSurface
   * <li>GeometryCollection -> GM_MultiPrimitive
   * </ul>
   * <p>
   * 
   * @param geometry
   *            the JTS- <tt>Geometry</tt> to be converted
   * @return the corresponding <tt>GM_Object</tt>
   * @throws GM_Exception
   *             if type unsupported or conversion failed
   */
  public static GM_Object wrap( final Geometry geometry ) throws GM_Exception
  {
    if( geometry instanceof Point )
      return wrap( (Point) geometry );

    if( geometry instanceof MultiPoint )
      return wrap( (MultiPoint) geometry );

    if( geometry instanceof LineString )
      return wrap( (LineString) geometry );

    if( geometry instanceof MultiLineString )
      return wrap( (MultiLineString) geometry );

    if( geometry instanceof Polygon )
      return wrap( (Polygon) geometry );

    if( geometry instanceof MultiPolygon )
      return wrap( (MultiPolygon) geometry );

    if( geometry instanceof GeometryCollection )
      return wrap( (GeometryCollection) geometry );

    throw new GM_Exception( "JTSAdapter.wrap does not support type '" + geometry.getClass().getName() + "'!" );
  }

  /**
   * Converts a <tt>GM_Point</tt> to a <tt>Point</tt>.
   * <p>
   * 
   * @param gmPoint
   *            point to be converted
   * @return the corresponding <tt>Point</tt> object
   */
  private static Point export( final GM_Point gmPoint )
  {
    final Coordinate coord = export( gmPoint.getPosition() );
    return jtsFactory.createPoint( coord );
  }

  /**
   * Converts a {@link GM_Position} to as {@link Coordinate}.
   * <p>
   * Distinguished between positions with 2 or 3 coordinates.
   * </p>
   */
  public static Coordinate export( final GM_Position pos )
  {
    final double[] asArray = pos.getAsArray();
    if( asArray.length > 2 )
      return new Coordinate( asArray[0], asArray[1], asArray[2] );
    return new Coordinate( asArray[0], asArray[1] );
  }

  public static GM_Position wrap( final Coordinate coord )
  {
    if( Double.isNaN( coord.z ) )
      return new GM_Position_Impl( coord.x, coord.y );

    return new GM_Position_Impl( coord.x, coord.y, coord.z );
  }

  /**
   * Converts a <tt>GM_MultiPoint</tt> to a <tt>MultiPoint</tt>.
   * <p>
   * 
   * @param gmMultiPoint
   *            multipoint to be converted
   * @return the corresponding <tt>MultiPoint</tt> object
   */
  private static MultiPoint export( final GM_MultiPoint gmMultiPoint )
  {
    final GM_Point[] gmPoints = gmMultiPoint.getAllPoints();
    final Point[] points = new Point[gmPoints.length];
    for( int i = 0; i < points.length; i++ )
    {
      points[i] = export( gmPoints[i] );
    }
    return jtsFactory.createMultiPoint( points );
  }

  /**
   * Converts a <tt>GM_Curve</tt> to a <tt>LineString</tt>.
   * <p>
   * 
   * @param curve
   *            <tt>GM_Curve</tt> to be converted
   * @return the corresponding <tt>LineString</tt> object
   * @throws GM_Exception
   */
  private static LineString export( final GM_Curve curve ) throws GM_Exception
  {
    final GM_LineString lineString = curve.getAsLineString();
    final Coordinate[] coords = new Coordinate[lineString.getNumberOfPoints()];
    for( int i = 0; i < coords.length; i++ )
    {
      final GM_Position position = lineString.getPositionAt( i );
      coords[i] = export( position );
    }
    return jtsFactory.createLineString( coords );
  }

  /**
   * Converts a <tt>GM_MultiCurve</tt> to a <tt>MultiLineString</tt>.
   * <p>
   * 
   * @param multi
   *            <tt>GM_MultiCurve</tt> to be converted
   * @return the corresponding <tt>MultiLineString</tt> object
   * @throws GM_Exception
   */
  private static MultiLineString export( final GM_MultiCurve multi ) throws GM_Exception
  {

    final GM_Curve[] curves = multi.getAllCurves();
    final LineString[] lineStrings = new LineString[curves.length];
    for( int i = 0; i < curves.length; i++ )
    {
      lineStrings[i] = export( curves[i] );
    }
    return jtsFactory.createMultiLineString( lineStrings );
  }

  /**
   * Converts an array of <tt>GM_Position</tt> s to a <tt>LinearRing</tt>.
   * <p>
   * 
   * @param positions
   *            an array of <tt>GM_Position</tt> s
   * @return the corresponding <tt>LinearRing</tt> object
   */
  private static LinearRing exportAsRing( final GM_Position[] positions )
  {
    final Coordinate[] coords = export( positions );
    return jtsFactory.createLinearRing( coords );
  }

  public static Coordinate[] export( final GM_Position[] positions )
  {
    final Coordinate[] coords = new Coordinate[positions.length];
    for( int i = 0; i < positions.length; i++ )
      coords[i] = export( positions[i] );
    return coords;
  }

  /**
   * Converts a <tt>GM_Surface</tt> to a <tt>Polygon</tt>.
   * <p>
   * Currently, the <tt>GM_Surface</tt> _must_ contain exactly one patch!
   * <p>
   * 
   * @param surface
   *            a <tt>GM_Surface</tt>
   * @return the corresponding <tt>Polygon</tt> object
   */
  private static Polygon export( final GM_Surface< ? > surface )
  {
    final GM_SurfacePatch patch = surface.get( 0 );
    final GM_Position[] exteriorRing = patch.getExteriorRing();
    final GM_Position[][] interiorRings = patch.getInteriorRings();

    final LinearRing shell = exportAsRing( exteriorRing );
    LinearRing[] holes = new LinearRing[0];
    if( interiorRings != null )
      holes = new LinearRing[interiorRings.length];
    for( int i = 0; i < holes.length; i++ )
      holes[i] = exportAsRing( interiorRings[i] );

    return jtsFactory.createPolygon( shell, holes );
  }

  /**
   * Converts a <tt>GM_MultiSurface</tt> to a <tt>MultiPolygon</tt>.
   * <p>
   * Currently, the contained <tt>GM_Surface</tt> _must_ have exactly one patch!
   * <p>
   * 
   * @param msurface
   *            a <tt>GM_MultiSurface</tt>
   * @return the corresponding <tt>MultiPolygon</tt> object
   */
  private static MultiPolygon export( final GM_MultiSurface msurface )
  {
    final GM_Surface< ? >[] surfaces = msurface.getAllSurfaces();
    final Polygon[] polygons = new Polygon[surfaces.length];

    for( int i = 0; i < surfaces.length; i++ )
    {
      polygons[i] = export( surfaces[i] );
    }
    return jtsFactory.createMultiPolygon( polygons );
  }

  /**
   * Converts a <tt>GM_MultiPrimitive</tt> to a <tt>GeometryCollection</tt>.
   * <p>
   * 
   * @param multi
   *            a <tt>GM_MultiPrimtive</tt>
   * @return the corresponding <tt>GeometryCollection</tt> object
   * @throws GM_Exception
   */
  private static GeometryCollection export( final GM_MultiPrimitive multi ) throws GM_Exception
  {
    final GM_Object[] primitives = multi.getAllPrimitives();
    final Geometry[] geometries = new Geometry[primitives.length];

    for( int i = 0; i < primitives.length; i++ )
    {
      geometries[i] = export( primitives[i] );
    }
    return jtsFactory.createGeometryCollection( geometries );
  }

  /**
   * Converts a <tt>Point</tt> to a <tt>GM_Point</tt>s.
   * <p>
   * 
   * @param point
   *            a <tt>Point</tt> object
   * @return the corresponding <tt>GM_Point</tt>
   */
  private static GM_Point wrap( final Point point )
  {
    final Coordinate coord = point.getCoordinate();
    if( Double.isNaN( coord.z ) )
      return new GM_Point_Impl( coord.x, coord.y, null );

    return new GM_Point_Impl( coord.x, coord.y, coord.z, null );
  }

  /**
   * Converts a <tt>MultiPoint</tt> to a <tt>GM_MultiPoint</tt>.
   * <p>
   * 
   * @param multi
   *            a <tt>MultiPoint</tt> object
   * @return the corresponding <tt>GM_MultiPoint</tt>
   */
  private static GM_MultiPoint wrap( final MultiPoint multi )
  {
    final GM_Point[] gmPoints = new GM_Point[multi.getNumGeometries()];
    for( int i = 0; i < gmPoints.length; i++ )
    {
      gmPoints[i] = wrap( (Point) multi.getGeometryN( i ) );
    }
    return new GM_MultiPoint_Impl( gmPoints, null );
  }

  /**
   * Converts a <tt>LineString</tt> to a <tt>GM_Curve</tt>.
   * <p>
   * 
   * @param line
   *            a <tt>LineString</tt> object
   * @return the corresponding <tt>GM_Curve</tt>
   * @throws GM_Exception
   */
  private static GM_Curve wrap( final LineString line ) throws GM_Exception
  {
    final Coordinate[] coords = line.getCoordinates();
    final GM_Position[] positions = new GM_Position[coords.length];
    for( int i = 0; i < coords.length; i++ )
    {
      if( !Double.isNaN( coords[i].z ) )
        positions[i] = new GM_Position_Impl( coords[i].x, coords[i].y, coords[i].z );
      else
        positions[i] = new GM_Position_Impl( coords[i].x, coords[i].y );
    }
    return GeometryFactory.createGM_Curve( positions, null );
  }

  /**
   * Converts a <tt>MultiLineString</tt> to a <tt>GM_MultiCurve</tt>.
   * <p>
   * 
   * @param multi
   *            a <tt>MultiLineString</tt> object
   * @return the corresponding <tt>GM_MultiCurve</tt>
   * @throws GM_Exception
   */
  private static GM_MultiCurve wrap( final MultiLineString multi ) throws GM_Exception
  {
    final GM_Curve[] curves = new GM_Curve[multi.getNumGeometries()];
    for( int i = 0; i < curves.length; i++ )
    {
      curves[i] = wrap( (LineString) multi.getGeometryN( i ) );
    }
    return GeometryFactory.createGM_MultiCurve( curves );
  }

  /**
   * Converts a <tt>Polygon</tt> to a <tt>GM_Surface</tt>.
   * <p>
   * 
   * @param polygon
   *            a <tt>Polygon</tt>
   * @return the corresponding <tt>GM_Surface</tt> object
   * @throws GM_Exception
   */
  private static GM_Surface<GM_Polygon> wrap( final Polygon polygon ) throws GM_Exception
  {
    final GM_Position[] exteriorRing = createGMPositions( polygon.getExteriorRing() );
    final GM_Position[][] interiorRings = new GM_Position[polygon.getNumInteriorRing()][];

    for( int i = 0; i < interiorRings.length; i++ )
    {
      interiorRings[i] = createGMPositions( polygon.getInteriorRingN( i ) );
    }
    final GM_Polygon patch = new GM_Polygon_Impl( new GM_SurfaceInterpolation_Impl(), exteriorRing, interiorRings, null );

    return new GM_Surface_Impl<GM_Polygon>( patch );
  }

  /**
   * Converts a <tt>MultiPolygon</tt> to a <tt>GM_MultiSurface</tt>.
   * <p>
   * 
   * @param multiPolygon
   *            a <tt>MultiPolygon</tt>
   * @return the corresponding <tt>GM_MultiSurface</tt> object
   * @throws GM_Exception
   */
  private static GM_MultiSurface wrap( final MultiPolygon multiPolygon ) throws GM_Exception
  {
    final GM_Surface< ? >[] surfaces = new GM_Surface[multiPolygon.getNumGeometries()];
    for( int i = 0; i < surfaces.length; i++ )
    {
      surfaces[i] = wrap( (Polygon) multiPolygon.getGeometryN( i ) );
    }
    return new GM_MultiSurface_Impl( surfaces );
  }

  /**
   * Converts a <tt>GeometryCollection</tt> to a <tt>GM_MultiPrimitve</tt>.
   * <p>
   * 
   * @param collection
   *            a <tt>GeometryCollection</tt>
   * @return the corresponding <tt>GM_MultiPrimitive</tt> object
   * @throws GM_Exception
   */
  private static GM_MultiPrimitive wrap( final GeometryCollection collection ) throws GM_Exception
  {

    final GM_MultiPrimitive multi = new GM_MultiPrimitive_Impl( null );
    for( int i = 0; i < collection.getNumGeometries(); i++ )
    {
      multi.setObjectAt( wrap( collection.getGeometryN( i ) ), i );
    }
    return multi;
  }

  /**
   * Converts a <tt>LineString</tt> to an array of <tt>GM_Position</tt>s.
   * <p>
   * 
   * @param line
   *            a <tt>LineString</tt> object
   * @return the corresponding array of <tt>GM_Position</tt> s
   */
  private static GM_Position[] createGMPositions( final LineString line )
  {
    final Coordinate[] coords = line.getCoordinates();
    final GM_Position[] positions = new GM_Position[coords.length];
    for( int i = 0; i < coords.length; i++ )
    {
      if( Double.isNaN( coords[i].z ) )
        positions[i] = new GM_Position_Impl( coords[i].x, coords[i].y );
      else
        positions[i] = new GM_Position_Impl( coords[i].x, coords[i].y, coords[i].z );
    }
    return positions;
  }

  public static GM_Envelope wrap( final Envelope env )
  {
    if( env.isNull() )
      return null;

    final Coordinate crdMin = new Coordinate( env.getMinX(), env.getMinY() );
    final Coordinate crdMax = new Coordinate( env.getMaxX(), env.getMaxY() );

    final GM_Position min = wrap( crdMin );
    final GM_Position max = wrap( crdMax );
    return new GM_Envelope_Impl( min, max, null );
  }

  public static Envelope export( final GM_Envelope env )
  {
    if( env == null )
      return null;

    final GM_Position posMin = env.getMin();
    final GM_Position posMax = env.getMax();

    final Coordinate min = export( posMin );
    final Coordinate max = export( posMax );
    return new Envelope( min, max );
  }

  public static LineString exportAsLineString( final GM_Position[] positions )
  {
    final Coordinate[] crds = export( positions );
    return jtsFactory.createLineString( crds );
  }

}