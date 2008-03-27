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

import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.j3d.geom.TriangulationUtils;
import org.kalypso.commons.xml.NS;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
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
    final double[] asArray1 = p1.getAsArray();
    final double[] asArray2 = p2.getAsArray();
    final int length = Math.min( asArray1.length, asArray2.length );
    final double[] newArray = new double[length];
    for( int i = 0; i < length; i++ )
      newArray[i] = (asArray1[i] + asArray2[i]) / 2d;

    return GeometryFactory.createGM_Position( newArray );
  }

  /**
   * assuming p1 and p2 have same coordinate system
   */
  public static GM_Point createGM_PositionAtCenter( final GM_Point p1, final GM_Point p2 )
  {
    final GM_Position newPos = createGM_PositionAtCenter( p1.getPosition(), p2.getPosition() );
    return GeometryFactory.createGM_Point( newPos, p1.getCoordinateSystem() );
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
    return getPolygonClass().isAssignableFrom( ftp.getValueClass() );
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

  public static Class< ? extends GM_Object> getPointClass( )
  {
    return GM_Point.class;
  }

  public static Class< ? extends GM_Object> getMultiPointClass( )
  {
    return GM_MultiPoint.class;
  }

  public static Class< ? extends GM_Object> getLineStringClass( )
  {
    return GM_Curve.class;
  }

  public static Class< ? extends GM_Object> getMultiLineStringClass( )
  {
    return GM_MultiCurve.class;
  }

  public static Class< ? extends GM_Object> getPolygonClass( )
  {
    return GM_Surface.class;
  }

  public static Class< ? extends GM_Object> getMultiPolygonClass( )
  {
    return GM_MultiSurface.class;
  }

  public static Class< ? extends GM_Object> getUndefinedGeometryClass( )
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
   * @param positions
   *            array of ordered {@link GM_Position}, last must equal first one
   * @return signed area, area >= 0 means points are counter clockwise defined (mathematic positive)
   */
  public static double calcSignedAreaOfRing( final GM_Position[] positions )
  {
    if( positions.length < 4 ) // 3 points and 4. is repetition of first point
      throw new UnsupportedOperationException( "can not calculate area of < 3 points" );
    final GM_Position a = positions[0]; // base
    double area = 0;
    for( int i = 1; i < positions.length - 2; i++ )
    {
      final GM_Position b = positions[i];
      final GM_Position c = positions[i + 1];
      area += (b.getY() - a.getY()) * (a.getX() - c.getX()) // bounding rectangle

          - ((a.getX() - b.getX()) * (b.getY() - a.getY())//
              + (b.getX() - c.getX()) * (b.getY() - c.getY())//
          + (a.getX() - c.getX()) * (c.getY() - a.getY())//
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
    final QName defaultGeometryPropertyQName = new QName( NS.GML3, "location" );
    final IValuePropertyType[] allGeomteryProperties = featureType.getAllGeomteryProperties();

    IValuePropertyType geometryProperty = null;
    for( final IValuePropertyType property : allGeomteryProperties )
    {
      if( aPreferedGeometryClass == null || property.getValueClass().isAssignableFrom( aPreferedGeometryClass ) )
      {
        geometryProperty = property;
        if( !geometryProperty.getQName().equals( defaultGeometryPropertyQName ) )
          break;
      }
    }
    return geometryProperty;
  }

  /** Return the envelope for the given geometry. If its a point, return the singular envelope containing this point. */
  public static GM_Envelope getEnvelope( final GM_Object geometry )
  {
    if( geometry instanceof GM_Point )
    {
      final GM_Position pos = ((GM_Point) geometry).getPosition();
      return GeometryFactory.createGM_Envelope( pos, pos, geometry.getCoordinateSystem() );
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
    final String crs = newLine.getCoordinateSystem();
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

  public static final GM_Envelope grabEnvelopeFromDistance( final GM_Point position, final double grabDistance )
  {
    final double posX = position.getX();
    final double posY = position.getY();
    final double grabDistanceHalf = grabDistance / 2;

    final GM_Position minPos = GeometryFactory.createGM_Position( posX - grabDistanceHalf, posY - grabDistanceHalf );
    final GM_Position maxPos = GeometryFactory.createGM_Position( posX + grabDistanceHalf, posY + grabDistanceHalf );

    final GM_Envelope reqEnvelope = GeometryFactory.createGM_Envelope( minPos, maxPos, position.getCoordinateSystem() );
    return reqEnvelope;
  }

  @SuppressWarnings( { "unchecked" })
  public static Feature findNearestFeature( final GM_Point point, final double grabDistance, final FeatureList modelList, final QName geoQName )
  {
    final GM_Envelope reqEnvelope = GeometryUtilities.grabEnvelopeFromDistance( point, grabDistance );
    final List<Feature> foundElements = modelList.query( reqEnvelope, null );

    double min = Double.MAX_VALUE;
    Feature nearest = null;

    for( final Feature feature : foundElements )
    {
      final GM_Object geom = (GM_Object) feature.getProperty( geoQName );

      if( geom != null )
      {
        final double curDist = point.distance( geom );
        if( min > curDist && curDist <= grabDistance )
        {
          nearest = feature;
          min = curDist;
        }
      }
    }
    return nearest;
  }

  /**
   * Same as {@link #findNearestFeature(GM_Point, double, FeatureList, QName)}, but only regards features of certain
   * qnames.
   * 
   * @param allowedQNames
   *            Only features that substitute one of these qnames are considered.
   */
  @SuppressWarnings("unchecked")
  public static Feature findNearestFeature( final GM_Point point, final double grabDistance, final FeatureList modelList, final QName geoQName, final QName[] allowedQNames )
  {
    final GM_Envelope reqEnvelope = GeometryUtilities.grabEnvelopeFromDistance( point, grabDistance );
    final List<Feature> foundElements = modelList.query( reqEnvelope, null );

    double min = Double.MAX_VALUE;
    Feature nearest = null;

    for( final Feature feature : foundElements )
    {
      if( GMLSchemaUtilities.substitutes( feature.getFeatureType(), allowedQNames ) )
      {
        final GM_Object geom = (GM_Object) feature.getProperty( geoQName );

        if( geom != null )
        {
          final double curDist = point.distance( geom );
          if( min > curDist && curDist <= grabDistance )
          {
            nearest = feature;
            min = curDist;
          }
        }
      }
    }
    return nearest;
  }

  /**
   * Calculates the direction (in degrees) from one position to another.
   * 
   * @return The angle in degree or {@link Double#NaN} if the points coincide.
   */
  public static double directionFromPositions( final GM_Position from, final GM_Position to )
  {
    final double vx = to.getX() - from.getX();
    final double vy = to.getY() - from.getY();

    return directionFromVector( vx, vy );
  }

  /**
   * Calculates the 'direction' of a vector in degrees. The degree value represents the angle between the vector and the
   * x-Axis in coordinate space.
   * <p>
   * Orientation is anti.clockwise (i.e. positive).
   * </p>
   * 
   * @return The angle in degree or {@link Double#NaN} if the given vector has length 0.
   */
  public static double directionFromVector( final double vx, final double vy )
  {
    final double length = Math.sqrt( vx * vx + vy * vy );
    if( length == 0.0 ) // double comparison problems?
      return Double.NaN;

    final double alpha = Math.acos( vx / length );

    if( vy < 0 )
      return Math.toDegrees( 2 * Math.PI - alpha );

    return Math.toDegrees( alpha );
  }

  /**
   * Scales an envelope by the given factor (1 means no scaling) while maintaining the position of its center-point.
   */
  public static GM_Envelope scaleEnvelope( final GM_Envelope zoomBox, final double factor )
  {
    final GM_Position zoomMax = zoomBox.getMax();
    final GM_Position zoomMin = zoomBox.getMin();

    final double newMaxX = zoomMin.getX() + (zoomMax.getX() - zoomMin.getX()) * factor;
    final double newMinX = zoomMax.getX() - (zoomMax.getX() - zoomMin.getX()) * factor;

    final double newMaxY = zoomMin.getY() + (zoomMax.getY() - zoomMin.getY()) * factor;
    final double newMinY = zoomMax.getY() - (zoomMax.getY() - zoomMin.getY()) * factor;

    final GM_Position newMin = GeometryFactory.createGM_Position( newMinX, newMinY );
    final GM_Position newMax = GeometryFactory.createGM_Position( newMaxX, newMaxY );

    return GeometryFactory.createGM_Envelope( newMin, newMax, zoomBox.getCoordinateSystem() );
  }

  /**
   * checks, if a position lies inside or outside of an polygon defined by a position array
   * 
   * @param pos
   *            position array of the polygon object
   * @param position
   *            position to be checked
   * @return 0 - if position lies outside of the polygon<BR>
   *         1 - if position lies inside of the polygon<BR>
   *         2 - if position lies on polygon's border.
   */
  public static int pointInsideOrOutside( final GM_Position[] pos, final GM_Position position )
  {
    int hits = 0;

    for( int i = 0; i < pos.length - 1; i++ )
    {
      /* check, if position lies on ring's border */
      final double sC = pos[i].getDistance( pos[i + 1] );
      final double sA = pos[i].getDistance( position );
      final double sB = pos[i + 1].getDistance( position );

      if( Math.abs( sC - sA - sB ) < 0.001 )
        return 2;

      /* calculate determinant */
      final double a00 = 2181.2838;
      final double a10 = 0.31415926; // = PI/10 (??)
      final double a01 = pos[i].getX() - pos[i + 1].getX();
      final double a11 = pos[i].getY() - pos[i + 1].getY();
      final double b0 = pos[i].getX() - position.getX();
      final double b1 = pos[i].getY() - position.getY();

      final double det = a00 * a11 - a10 * a01;
      if( det == 0.0 )
      {
        System.out.println( "Indefinite problem in pointInsideOrOutside" );
      }
      final double x0 = (a11 * b0 - a01 * b1) / det;
      final double x1 = (-a10 * b0 + a00 * b1) / det;

      if( x0 > 0. && x1 >= 0. && x1 <= 1. )
        hits++;
    }
    final int check = hits % 2;

    if( check == 1 )
      return 1;

    return 0;
  }

  /**
   * Convert the given bounding box into a {@link GM_Curve}
   */
  public static final GM_Curve toGM_Curve( final GM_Envelope bBox, final String crs )
  {
    try
    {
      final GM_Position min = bBox.getMin();
      final GM_Position max = bBox.getMax();

      final double minx = min.getX();
      final double miny = min.getY();

      final double maxx = max.getX();
      final double maxy = max.getY();

      final double[] coords = new double[] { minx, miny, maxx, miny, maxx, maxy, minx, maxy, minx, miny, };
      final GM_Curve curve = GeometryFactory.createGM_Curve( coords, 2, crs );
      return curve;
    }
    catch( final Throwable e )
    {
      throw new RuntimeException( "error while creating a curve", e ); //$NON-NLS-1$
    }
  }

  /**
   * Tests whether a ring (defined by its positions) is self-intersecting.
   */
  public static boolean isSelfIntersecting( final GM_Position[] ring )
  {
    final LineString ls = JTSAdapter.exportAsLineString( ring );
    return !ls.isSimple();
  }

  protected static GM_Position[] getPolygonPositions( final GM_Curve[] curves, final boolean selfIntersected ) throws GM_Exception
  {
    /* test for self-intersection */
    final List<GM_Position> posList = new ArrayList<GM_Position>();

    /* - add first curve's positions to positions list */
    final GM_Position[] positions1 = curves[1].getAsLineString().getPositions();
    for( final GM_Position element : positions1 )
    {
      posList.add( element );
    }

    /* - add second curve's positions to positions list */
    final GM_Position[] positions2 = curves[0].getAsLineString().getPositions();

    if( selfIntersected != true )
    {
      // not twisted: curves are oriented in the same direction, so we add the second curve's positions in the
      // opposite direction in order to get a non-self-intersected polygon.
      for( int i = 0; i < positions2.length; i++ )
      {
        posList.add( positions2[positions2.length - 1 - i] );
      }
    }
    else
    {
      // twisted: curves are oriented in different directions, so we add the second curve's positions
      // from start to end in order to get a non-self-intersected polygon.
      for( final GM_Position element : positions2 )
      {
        posList.add( element );
      }
    }

    /* close polygon position list */
    posList.add( positions1[0] );

    return posList.toArray( new GM_Position[posList.size()] );
  }

  /**
   * converts two given curves into a position array of a ccw oriented, closed polygon.<br>
   * The ring is simply produced by adding all positions of the first curve and the positions of the second curve in
   * inverse order.
   * 
   * @param curves
   *            the curves as {@link GM_Curve}
   */
  public static GM_Position[] getPolygonfromCurves( final GM_Curve firstCurve, final GM_Curve secondCurve ) throws GM_Exception
  {
    /* get the positions of the curves */

    // as a first guess, we assume that the curves build a non-intersecting polygon
    final GM_Position[] firstPoses = firstCurve.getAsLineString().getPositions();
    final GM_Position[] secondPoses = secondCurve.getAsLineString().getPositions();
    final GM_Position[] polygonPositions = new GM_Position[firstPoses.length + secondPoses.length];

    for( int i = 0; i < firstPoses.length; i++ )
      polygonPositions[i] = firstPoses[i];

    for( int i = 0; i < secondPoses.length; i++ )
      polygonPositions[i + firstPoses.length] = secondPoses[secondPoses.length - i - 1];

    return orientateRing( polygonPositions );
  }

  /**
   * converts two given curves into a position array of a non-self-intersecting, ccw oriented, closed polygon
   * 
   * @param curves
   *            the curves as {@link GM_Curve}
   */
  public static GM_Position[] getPolygonfromCurves( final GM_Curve[] curves ) throws GM_Exception
  {
    /* get the positions of the curves */

    // as a first guess, we assume that the curves build a non-intersecting polygon
    GM_Position[] polygonPositions = getPolygonPositions( curves, false );

    // then we check this assumption
    if( isSelfIntersecting( polygonPositions ) )
      polygonPositions = getPolygonPositions( curves, true );

    return orientateRing( polygonPositions );
  }

  /**
   * Orientates a ring counter clock wise.
   * 
   * @return The inverted list of position, or the original list, if the ring was already oriented in the right way.
   */
  public static GM_Position[] orientateRing( final GM_Position[] polygonPositions )
  {
    // check orientation
    if( calcSignedAreaOfRing( polygonPositions ) < 0 )
    {
      /* orientation is cw */
      // invert the direction
      final GM_Position[] invertedPositions = new GM_Position[polygonPositions.length];
      for( int i = 0; i < polygonPositions.length; i++ )
      {
        invertedPositions[i] = polygonPositions[polygonPositions.length - 1 - i];
      }
      return invertedPositions;
    }
    else
      return polygonPositions;
  }

  /**
   * Triangulates a closed ring (must be oriented counter-clock-wise).
   * 
   * @return An array of triangles: GM_Position[numberOfTriangles][3]
   */
  public static GM_Position[][] triangulateRing( final GM_Position[] ring )
  {
    final float[] posArray = new float[ring.length * 3];

    for( int i = 0; i < ring.length; i++ )
    {
      posArray[i * 3] = (float) ring[i].getX();
      posArray[i * 3 + 1] = (float) ring[i].getY();
      posArray[i * 3 + 2] = (float) ring[i].getZ();
    }

    final float[] normal = { 0, 0, 1 };
    final int[] output = new int[posArray.length];
    final int numVertices = posArray.length / 3;
    final TriangulationUtils triangulator = new TriangulationUtils();
    final int num = triangulator.triangulateConcavePolygon( posArray, 0, numVertices, output, normal );

    final GM_Position[][] triangles = new GM_Position[num][3];

    for( int i = 0; i < num; i++ )
    {
      triangles[i] = new GM_Position[3];

      final double x1 = posArray[output[i * 3]];
      final double y1 = posArray[output[i * 3] + 1];
      final double z1 = posArray[output[i * 3] + 2];
      final double x2 = posArray[output[i * 3 + 1]];
      final double y2 = posArray[output[i * 3 + 1] + 1];
      final double z2 = posArray[output[i * 3 + 1] + 2];
      final double x3 = posArray[output[i * 3 + 2]];
      final double y3 = posArray[output[i * 3 + 2] + 1];
      final double z3 = posArray[output[i * 3 + 2] + 2];
      triangles[i][0] = GeometryFactory.createGM_Position( x1, y1, z1 );
      triangles[i][1] = GeometryFactory.createGM_Position( x2, y2, z2 );
      triangles[i][2] = GeometryFactory.createGM_Position( x3, y3, z3 );
    }

    return triangles;
  }

}