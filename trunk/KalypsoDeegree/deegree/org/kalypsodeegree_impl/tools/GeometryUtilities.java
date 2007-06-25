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
package org.kalypsodeegree_impl.tools;

import javax.xml.namespace.QName;

import org.kalypso.commons.xml.NS;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
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
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Primitive;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;
import org.opengis.cs.CS_CoordinateSystem;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.simplify.DouglasPeuckerLineSimplifier;

/**
 * @author doemming
 */
public class GeometryUtilities
{
  public static final QName QN_POLYGON = new QName( NS.GML3, "Polygon" );

  public static final QName QN_POLYGON_PROPERTY = new QName( NS.GML3, "PolygonPropertyType" );

  public static final QName QN_POINT = new QName( NS.GML3, "Point" );

  public static final QName QN_POINT_PROPERTY = new QName( NS.GML3, "PointPropertyType" );

  public static final QName QN_LINE_STRING = new QName( NS.GML3, "LineString" );

  public static final QName QN_LINE_STRING_PROPERTY = new QName( NS.GML3, "LineStringPropertyType" );

  public static final QName QN_MULTI_POINT = new QName( NS.GML3, "MultiPoint" );

  public static final QName QN_MULTI_POINT_PROPERTY = new QName( NS.GML3, "MultiPointPropertyType" );

  public static final QName QN_MULTI_LINE_STRING = new QName( NS.GML3, "MultiLineString" );

  public static final QName QN_MULTI_LINE_STRING_PROPERTY = new QName( NS.GML3, "MultiLineStringPropertyType" );

  public static final QName QN_MULTI_POLYGON = new QName( NS.GML3, "MultiPolygon" );

  public static final QName QN_MULTI_POLYGON_PROPERTY = new QName( NS.GML3, "MultiPolygonPropertyType" );

  public static final QName QN_LOCATION = new QName( NS.GML3, "location" );

  public static final QName QN_LOCATION_PROPERTY = new QName( NS.GML3, "LocationPropertyType" );

  public static final QName QN_DIRECTION = new QName( NS.GML3, "direction" );

  public static final QName QN_DIRECTION_PROPERTY = new QName( NS.GML3, "DirectionPropertyType" );

  public GeometryUtilities( )
  {
    super();
  }

  public static GM_Curve createArrowLineString( final GM_Point srcP, final GM_Point targetP ) throws GM_Exception
  {
    final GM_Position[] pos = new GM_Position[] { srcP.getPosition(), targetP.getPosition() };
    return GeometryFactory.createGM_Curve( pos, srcP.getCoordinateSystem() );
  }

  public static GM_Curve createArrowLineString( final GM_Point srcP, final GM_Point targetP, final double weightLength, final double weightWidth ) throws GM_Exception
  {
    final double dx = targetP.getX() - srcP.getX();
    final double dy = targetP.getY() - srcP.getY();

    final GM_Position p1 = srcP.getPosition();
    final GM_Position p4 = targetP.getPosition();
    final GM_Position p2 = GeometryFactory.createGM_Position( p1.getX() + weightLength * dx, p1.getY() + weightLength * dy );
    final GM_Position p3 = GeometryFactory.createGM_Position( p2.getX() + weightWidth * dy, p2.getY() - weightWidth * dx );
    final GM_Position p5 = GeometryFactory.createGM_Position( p2.getX() - weightWidth * dy, p2.getY() + weightWidth * dx );

    final GM_Position[] pos = new GM_Position[] { p1, p2, p3, p4, p5, p2 };
    return GeometryFactory.createGM_Curve( pos, srcP.getCoordinateSystem() );
  }

  /**
   * creates a new GM_Position that is on the straight line defined with the positions and has a special distance from
   * basePoint in the direction towards the directionPoint
   */
  public static GM_Position createGM_PositionAt( final GM_Position basePoint, final GM_Position directionPoint, final double distanceFromBasePoint )
  {
    final double[] p1 = basePoint.getAsArray();
    final double distance = basePoint.getDistance( directionPoint );
    if( distance == 0 )
      return GeometryFactory.createGM_Position( p1 );
    final double[] p2 = directionPoint.getAsArray();
    final double factor = distanceFromBasePoint / distance;
    final double newPos[] = new double[p1.length];
    // for( int i = 0; i < newPos.length; i++ )
    for( int i = 0; i < 2; i++ )
      newPos[i] = p1[i] + (p2[i] - p1[i]) * factor;
    return GeometryFactory.createGM_Position( newPos );
  }

  public static GM_Position getGM_PositionBetweenAtLevel( final GM_Position p1, final GM_Position p2, final double iso )
  {
    final double dz1 = iso - p1.getZ();
    final double dz2 = p2.getZ() - p1.getZ();
    final double dx = p2.getX() - p1.getX();
    final double c = dz1 / dz2;
    // check between
    if( c < -0.01d || c > 1.01d )
      return null;
    return GeometryFactory.createGM_Position( p1.getX() + c * dx, p1.getY() + c * (p2.getY() - p1.getY()), iso );
  }

  public static GM_Position createGM_PositionAtCenter( final GM_Position p1, final GM_Position p2 )
  {
    return GeometryFactory.createGM_Position( (p1.getX() + p2.getX()) / 2d, (p1.getY() + p2.getY()) / 2d, (p1.getZ() + p2.getZ()) / 2d );
  }

  /**
   * assuming p1 and p2 have same coordinate system
   */
  public static GM_Point createGM_PositionAtCenter( final GM_Point p1, final GM_Point p2 )
  {
    return GeometryFactory.createGM_Point( (p1.getX() + p2.getX()) / 2d, (p1.getY() + p2.getY()) / 2d, p1.getCoordinateSystem() );
  }

  @SuppressWarnings("unchecked")
  public static double calcAngleToSurface( final GM_Surface surface, final GM_Point point )
  {
    final double r = surface.distance( point );
    double min = r;
    double resultAngle = 0;
    final double n = 8;
    for( double angle = 0; angle < 2d * Math.PI; angle += 2d * Math.PI / n )
    {
      final GM_Point p = createPointFrom( point, angle, r / 2 );
      final double distance = surface.distance( p );
      if( distance < min )
      {
        min = distance;
        resultAngle = angle;
      }
    }
    return resultAngle;
  }

  /**
   * guess point that is on the surface
   * 
   * @param surface
   *            surface that should contain the result point
   * @param pointGuess
   * @param tries
   *            numer of maximal interations
   * @return point that is somewhere on the surface (e.g. can act as label point)
   */
  @SuppressWarnings("unchecked")
  public static GM_Point guessPointOnSurface( final GM_Surface surface, GM_Point pointGuess, int tries )
  {
    if( surface == null )
      return null;
    if( pointGuess == null )
      pointGuess = surface.getCentroid();
    if( tries <= 0 )
      return pointGuess;
    tries--;
    if( surface.contains( pointGuess ) )
      return pointGuess;
    // 
    // pointGuess1
    // |
    // |radius1
    // |
    // --p1--- at border
    // ----------
    // ------------
    // ------------
    // --result----
    // -----------
    // ----------
    // -----------
    // -surface---
    // --p2------- at border
    // |
    // |
    // |
    // |
    // |radius2
    // |
    // |
    // |
    // |
    // pointGuess2
    //    
    // 1. find point at surface on one side
    final double angle1 = calcAngleToSurface( surface, pointGuess );
    final double r1 = surface.distance( pointGuess );
    final GM_Point p1 = createPointFrom( pointGuess, angle1, r1 );
    final GM_Point p2 = calcFarestPointOnSurfaceInDirection( surface, p1, angle1, Math.sqrt( Math.pow( surface.getEnvelope().getHeight(), 2 ) * Math.pow( surface.getEnvelope().getWidth(), 2 ) ), 8 );
    return guessPointOnSurface( surface, createGM_PositionAtCenter( p1, p2 ), tries );
  }

  @SuppressWarnings("unchecked")
  private static GM_Point calcFarestPointOnSurfaceInDirection( final GM_Surface surface, final GM_Point pOnSurface, final double angle, final double max, int tries )
  {
    final GM_Point point = createPointFrom( pOnSurface, angle, max );
    if( surface.contains( point ) )
      return point;
    if( tries <= 0 )
      return point;// return the best try
    tries--;
    final double distance = surface.distance( point );
    return calcFarestPointOnSurfaceInDirection( surface, pOnSurface, angle, max - distance, tries );
  }

  // public static GM_Point guessPointOnSurface( final GM_Surface surface, //
  // // // GM_Point firstGuessPoint, int tries )
  // {
  // if( surface == null )
  // return null;
  // if( firstGuessPoint == null )
  // firstGuessPoint = surface.getCentroid();
  // if( tries <= 0 )
  // return firstGuessPoint;
  // tries--;
  // //
  // // guessPoint
  // // |
  // // |radius
  // // |
  // // --p1-- at border
  // // --|-----
  // // --p3----- middle of p1 and p2
  // // --|------
  // // --p2---- guesspoint mirror at p2
  // // --------
  // // ---------
  // // -surface-
  // // ---------
  // //
  // // 1. find direction to surface
  // double n = 8; // number of directions to test
  // final double r = surface.distance( firstGuessPoint );
  // double min = r;
  // double resultAngle = 0;
  // for( double angle = 0; angle < 2d * Math.PI; angle += Math.PI / n )
  // {
  // GM_Point p = createPointFrom( firstGuessPoint, angle, r / 2 );
  // double distance = surface.distance( p );
  // if( distance < min )
  // {
  // min = distance;
  // resultAngle = angle;
  // }
  // }
  // // calc point at border
  // final GM_Point p1 = createPointFrom( firstGuessPoint, resultAngle, r );
  // // mirror at p1
  // final GM_Point p2 = createPointFrom( firstGuessPoint, resultAngle, 2 * r );
  // // center of p1 and p2
  // final GM_Point p3 = createGM_PositionAtCenter( p1, p2 );
  //    
  //    
  // if(!surface.contains( p2 ) )
  // {
  // if( surface.contains( p3 ) )
  // return p3;
  // return p2;
  // }
  // return guessPointOnSurface( surface, p3, tries );
  // // if( surface.contains( p2 ) )
  // // {
  // // if( surface.contains( p3 ) )
  // // return p3;
  // // return p2;
  // // }
  // // return guessPointOnSurface( surface, p3, tries );
  // }

  private static GM_Point createPointFrom( final GM_Point centroid, final double angle, final double radius )
  {
    final double x = centroid.getX() + Math.cos( angle ) * radius;
    final double y = centroid.getY() + Math.sin( angle ) * radius;
    return GeometryFactory.createGM_Point( x, y, centroid.getCoordinateSystem() );
  }

  @SuppressWarnings("unchecked")
  public static double calcArea( final GM_Object geom )
  {
    if( geom instanceof GM_Surface )
      return ((GM_Surface) geom).getArea();
    else if( geom instanceof GM_MultiSurface )
    {
      double area = 0;
      final GM_Surface[] allSurfaces = ((GM_MultiSurface) geom).getAllSurfaces();
      for( final GM_Surface element : allSurfaces )
        area += calcArea( element );
      return area;
    }
    else if( geom instanceof GM_MultiPrimitive )
    {
      double area = 0;
      final GM_Primitive[] allPrimitives = ((GM_MultiPrimitive) geom).getAllPrimitives();
      for( final GM_Primitive element : allPrimitives )
        area += calcArea( element );
      return area;
    }
    return 0d;
  }

  @SuppressWarnings("unchecked")
  public static boolean isInside( final GM_Object a, final GM_Object b )
  {
    if( a instanceof GM_Surface && b instanceof GM_Surface )
      return a.contains( guessPointOnSurface( (GM_Surface) b, b.getCentroid(), 3 ) );
    // return a.contains(b);
    if( a instanceof GM_MultiSurface )
      return isInside( ((GM_MultiSurface) a).getAllSurfaces()[0], b );
    if( b instanceof GM_MultiSurface )
      return isInside( a, ((GM_MultiSurface) b).getAllSurfaces()[0] );
    return false;
  }

  public static double calcArea( final GM_Envelope env )
  {
    return env.getHeight() * env.getHeight();
  }

  public static GM_Position createGM_PositionAverage( final GM_Position[] positions )
  {
    double x = 0d, y = 0d;
    for( final GM_Position position : positions )
    {
      x += position.getX();
      y += position.getY();
    }
    return GeometryFactory.createGM_Position( x / positions.length, y / positions.length );
  }

  /**
   * @return <code>true</code> if feature property type equals this type of geometry
   */
  public static boolean isPointGeometry( final IValuePropertyType ftp )
  {
    // remember to use the same classes as used by the marshalling type handlers !!
    return ftp.getValueClass().equals( getPointClass() );

  }

  /**
   * @param o
   * @return <code>true</code> if object type equals this type of geometry
   */
  public static boolean isPointGeometry( final Object o )
  {
    return o.getClass().equals( getPointClass() );
  }

  /**
   * @param ftp
   * @return <code>true</code> if feature property type equals this type of geometry
   */
  public static boolean isMultiPointGeometry( final IValuePropertyType ftp )
  {
    return ftp.getValueClass().equals( getMultiPointClass() );
  }

  /**
   * @param o
   * @return <code>true</code> if object type equals this type of geometry
   */
  public static boolean isMultiPointGeometry( final Object o )
  {
    return o.getClass().equals( getMultiPointClass() );
  }

  /**
   * @param ftp
   * @return <code>true</code> if feature property type equals this type of geometry
   */
  public static boolean isLineStringGeometry( final IValuePropertyType ftp )
  {
    return ftp.getValueClass().equals( getLineStringClass() );
  }

  /**
   * @param o
   * @return <code>true</code> if object type equals this type of geometry
   */

  public static boolean isLineStringGeometry( final Object o )
  {
    return o.getClass().equals( getLineStringClass() );
  }

  /**
   * @param ftp
   * @return <code>true</code> if feature property type equals this type of geometry
   */
  public static boolean isMultiLineStringGeometry( final IValuePropertyType ftp )
  {
    return ftp.getValueClass().equals( getMultiLineStringClass() );
  }

  /**
   * @param o
   * @return <code>true</code> if object type equals this type of geometry
   */
  public static boolean isMultiLineStringGeometry( final Object o )
  {
    return o.getClass().equals( getMultiLineStringClass() );
  }

  /**
   * @param ftp
   * @return <code>true</code> if feature property type equals this type of geometry
   */
  public static boolean isPolygonGeometry( final IValuePropertyType ftp )
  {
    return ftp.getValueClass().equals( getPolygonClass() );
  }

  /**
   * @param o
   * @return <code>true</code> if object type equals this type of geometry
   */
  public static boolean isPolygonGeometry( final Object o )
  {
    final Class< ? extends Object> class1 = o.getClass();
    return getPolygonClass().isAssignableFrom( class1 );
  }

  /**
   * @param ftp
   * @return <code>true</code> if feature property type equals this type of geometry
   */
  public static boolean isMultiPolygonGeometry( final IValuePropertyType ftp )
  {
    return ftp.getValueClass().equals( getMultiPolygonClass() );
  }

  /**
   * @param o
   * @return <code>true</code> if object type equals this type of geometry
   */
  public static boolean isMultiPolygonGeometry( final Object o )
  {
    return o.getClass().equals( getMultiPolygonClass() );
  }

  /**
   * @param ftp
   * @return <code>true</code> if feature property type equals this type of geometry
   */
  public static boolean isUndefinedGeometry( final IValuePropertyType ftp )
  {
    return ftp.getValueClass().equals( getUndefinedGeometryClass() );
  }

  /**
   * @param o
   * @return <code>true</code> if object type equals this type of geometry
   */
  public static boolean isUndefinedGeometry( final Object o )
  {
    return o.getClass().equals( getUndefinedGeometryClass() );
  }

  /**
   * @param ftp
   * @return <code>true</code> if feature property type equals this type of geometry
   */
  public static boolean isAnyMultiGeometry( final IPropertyType ftp )
  {
    ftp.getClass(); // no yellow things
    return false; // not supported TODO support it
  }

  public static boolean isEnvelopeGeometry( final IValuePropertyType ftp )
  {
    return getEnvelopeClass().equals( ftp.getValueClass() );
  }

  /**
   * @param o
   * @return <code>true</code> if object type equals this type of geometry
   */
  public static boolean isEnvelopeGeometry( final Object o )
  {
    return getEnvelopeClass().equals( o.getClass() );
  }

  public static Class< ? extends Object> getEnvelopeClass( )
  {
    return GM_Envelope.class;
  }

  public static boolean isGeometry( final IPropertyType pt )
  {
    if( !(pt instanceof IValuePropertyType) )
      return false;
    final IValuePropertyType gpt = (IValuePropertyType) pt;
    if( isPointGeometry( gpt ) )
      return true;
    if( isMultiPointGeometry( gpt ) )
      return true;
    if( isLineStringGeometry( gpt ) )
      return true;
    if( isMultiLineStringGeometry( gpt ) )
      return true;
    if( isPolygonGeometry( gpt ) )
      return true;
    if( isMultiPolygonGeometry( gpt ) )
      return true;
    if( isAnyMultiGeometry( gpt ) )
      return true;
    // if( isEnvelopeGeometry( ftp ) )
    // return true;
    return false;
  }

  // public static Class getClass( IPropertyType ftp )
  // {
  // if( isPointGeometry( ftp ) )
  // return getPointClass();
  // if( isMultiPointGeometry( ftp ) )
  // return getMultiPointClass();
  // if( isLineStringGeometry( ftp ) )
  // return getLineStringClass();
  // if( isMultiLineStringGeometry( ftp ) )
  // return getMultiLineStringClass();
  // if( isPolygonGeometry( ftp ) )
  // return getPolygonClass();
  // if( isMultiPolygonGeometry( ftp ) )
  // return getMultiPolygonClass();
  // if( isAnyMultiGeometry( ftp ) )
  // return null;
  // return null;
  // }

  public static Class< ? extends Object> getPointClass( )
  {
    return GM_Point.class;
  }

  public static Class< ? extends Object> getMultiPointClass( )
  {
    return GM_MultiPoint.class;
  }

  public static Class< ? extends Object> getLineStringClass( )
  {
    return GM_Curve.class;
  }

  public static Class< ? extends Object> getMultiLineStringClass( )
  {
    return GM_MultiCurve.class;
  }

  public static Class< ? extends Object> getPolygonClass( )
  {
    return GM_Surface.class;
  }

  public static Class< ? extends Object> getMultiPolygonClass( )
  {
    return GM_MultiSurface.class;
  }

  public static Class< ? extends Object> getUndefinedGeometryClass( )
  {
    return GM_Object.class;
  }

  public static boolean isGeometry( final Object o )
  {
    final Class< ? extends Object> class1 = o.getClass();
    if( getUndefinedGeometryClass().isAssignableFrom( class1 ) )
      return true;
    else if( getPointClass().isAssignableFrom( class1 ) )
      return true;
    else if( getMultiPointClass().isAssignableFrom( class1 ) )
      return true;
    else if( getLineStringClass().isAssignableFrom( class1 ) )
      return true;
    else if( getMultiLineStringClass().isAssignableFrom( class1 ) )
      return true;
    else if( getPolygonClass().isAssignableFrom( class1 ) )
      return true;
    else if( getMultiPolygonClass().isAssignableFrom( class1 ) )
      return true;
    return false;
  }

  /**
   * This method ensure to return a multi polygon (GM_MultiSurface ). the geomToCheck is a polygon ( GM_Surface) the
   * polygon is wrapped to a multi polygon.
   * 
   * @param geomToCheck
   *            geometry object to check
   * @return multi polygon, if geomToCheck is null, null is returned, if the geomToCheck is a multi polygon it returns
   *         itself
   * @exception a
   *                GM_Exception is thrown when a the geomToCheck can not be wrapped in a multi polygon.
   */
  @SuppressWarnings("unchecked")
  public static GM_MultiSurface ensureIsMultiPolygon( final GM_Object geomToCheck ) throws GM_Exception
  {
    final Class< ? extends GM_Object> class1 = geomToCheck.getClass();
    if( geomToCheck == null )
      return null;
    else if( getMultiPolygonClass().isAssignableFrom( class1 ) )
      return (GM_MultiSurface) geomToCheck;
    else if( getPolygonClass().isAssignableFrom( class1 ) )
      return GeometryFactory.createGM_MultiSurface( new GM_Surface[] { (GM_Surface) geomToCheck }, ((GM_Surface) geomToCheck).getCoordinateSystem() );
    else
      throw new GM_Exception( "This geometry can not be a MultiPolygon..." );
  }

  /**
   * @param ring
   *            array of ordered coordinates, last must equal first one
   * @return signed area, area >= 0 means points are counter clockwise defined (mathematic positive)
   */
  public static double calcSignedAreaOfRing( final Coordinate[] ring )
  {
    if( ring.length < 4 ) // 3 points and 4. is repetition of first point
      throw new UnsupportedOperationException( "can not calculate area of < 3 points" );
    final Coordinate a = ring[0]; // base
    double area = 0;
    for( int i = 1; i < ring.length - 2; i++ )
    {
      final Coordinate b = ring[i];
      final Coordinate c = ring[i + 1];
      area += (b.y - a.y) * (a.x - c.x) // bounding rectangle

          - ((a.x - b.x) * (b.y - a.y)//
              + (b.x - c.x) * (b.y - c.y)//
          + (a.x - c.x) * (c.y - a.y)//
          ) / 2d;
    }
    return area;
  }

  /**
   * Finds the first geometry property of the given feature type.
   * 
   * @param aPreferedGeometryClass
   *            If non null, the first property of this type is returned.
   */
  public static IValuePropertyType findGeometryProperty( final IFeatureType featureType, final Class< ? > aPreferedGeometryClass )
  {
    final IValuePropertyType[] allGeomteryProperties = featureType.getAllGeomteryProperties();

    IValuePropertyType geometryProperty = null;
    for( final IValuePropertyType property : allGeomteryProperties )
    {
      if( aPreferedGeometryClass == null || property.getValueClass() == aPreferedGeometryClass )
      {
        geometryProperty = property;
        break;
      }
    }
    return geometryProperty;
  }

  /** Return the envelope for the given geometry. If its a point, return the singular envelope continaing this point. */
  public static GM_Envelope getEnvelope( final GM_Object geometry )
  {
    if( geometry instanceof GM_Point )
    {
      final GM_Position pos = ((GM_Point) geometry).getPosition();
      return GeometryFactory.createGM_Envelope( pos, pos );
    }

    return geometry.getEnvelope();
  }

  /**
   * clones a GM_Linestring as GM_Curve and sets its z-value to a given value.
   * 
   * @param newLine
   *            the input linestring
   * @param value
   *            the new z-value
   */
  public static GM_Curve setValueZ( final GM_LineString newLine, final double value ) throws GM_Exception
  {
    final GM_Position[] positions = newLine.getPositions();
    final CS_CoordinateSystem crs = newLine.getCoordinateSystem();
    final GM_Position[] newPositions = new GM_Position[positions.length];
    for( int i = 0; i < positions.length; i++ )
    {
      final GM_Position position = positions[i];
      newPositions[i] = GeometryFactory.createGM_Position( position.getX(), position.getY(), value );
    }
    return GeometryFactory.createGM_Curve( newPositions, crs );
  }

  /**
   * creates a new curve by simplifying a given curve by using Douglas-Peucker Algorithm.
   * 
   * @param curve
   *            input curve to be simplified
   * @param epsThinning
   *            max. distance value for Douglas-Peucker-Algorithm
   */
  public static GM_Curve getThinnedCurve( final GM_Curve curve, final Double epsThinning ) throws GM_Exception
  {
    final LineString line = (LineString) JTSAdapter.export( curve );
    final Coordinate[] coordinates = line.getCoordinates();

    final Coordinate[] simplifiedCrds = DouglasPeuckerLineSimplifier.simplify( coordinates, epsThinning );
    final LineString simplifiedLine = line.getFactory().createLineString( simplifiedCrds );
    final GM_Curve thinnedCurve = (GM_Curve) JTSAdapter.wrap( simplifiedLine );
    return thinnedCurve;
  }

}