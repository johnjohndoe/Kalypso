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

import org.deegree.model.geometry.GM_Curve;
import org.deegree.model.geometry.GM_Exception;
import org.deegree.model.geometry.GM_MultiCurve;
import org.deegree.model.geometry.GM_MultiPoint;
import org.deegree.model.geometry.GM_MultiSurface;
import org.deegree.model.geometry.GM_Object;
import org.deegree.model.geometry.GM_Point;
import org.deegree.model.geometry.GM_Position;
import org.deegree.model.geometry.GM_Surface;
import org.deegree_impl.tools.Debug;
import org.opengis.cs.CS_CoordinateSystem;
import org.postgis.Geometry;
import org.postgis.LineString;
import org.postgis.LinearRing;
import org.postgis.MultiLineString;
import org.postgis.MultiPoint;
import org.postgis.MultiPolygon;
import org.postgis.PGgeometry;
import org.postgis.Point;
import org.postgis.Polygon;

/**
 * 
 * 
 * @version $Revision$
 * @author <a href="mailto:wanhoff@giub.uni-bonn.de">Jeronimo Wanhoff </a>
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 */
public class PostGISAdapter
{

  /**
   * creates a deegree geometry from the passed postgis geometry and CRS
   * 
   * @param geom
   *          postgis geometry
   * @param crs
   *          coordinate reference system
   * 
   * @return deegree geometry
   * 
   * @throws GM_Exception
   */
  public static GM_Object wrap( PGgeometry geom, CS_CoordinateSystem crs ) throws GM_Exception
  {

    // PGgeometries have type 1 to 7. Process geometries depending on type
    // in PostGIS
    switch( geom.getGeoType() )
    {
    case 1:
      return createPoint( (Point)geom.getGeometry(), crs );
    case 2:
      return createCurve( (LineString)geom.getGeometry(), crs );
    case 3:
      return createSurface( (Polygon)geom.getGeometry(), crs );
    case 4:
      return createMultiPoint( (MultiPoint)geom.getGeometry(), crs );
    case 5:
      return createMultiCurve( (MultiLineString)geom.getGeometry(), crs );
    case 6:
      return createMultiSurface( (MultiPolygon)geom.getGeometry(), crs );
    case 7:
      System.out.println( "GeometryCollection is not supported!" );
      return null;
    default:
      System.out.println( "something went wrong!!!" );
      return null;
    }
  }

  /**
   * exports the passed deegree geometry as the corresponding postgis geometry
   * 
   * @param geom
   *          deegree geometry
   * 
   * @return postgis geometry
   */
  public static Geometry export( GM_Object geom ) throws GM_Exception
  {
    Debug.debugMethodBegin( "PostGISAdapter", "export(GM_Object)" );

    Geometry pg = null;

    if( geom instanceof GM_Point )
    {
      pg = export( (GM_Point)geom );
    }
    else if( geom instanceof GM_Curve )
    {
      pg = export( (GM_Curve)geom );
    }
    else if( geom instanceof GM_Surface )
    {
      pg = export( (GM_Surface)geom );
    }
    else if( geom instanceof GM_MultiPoint )
    {
      pg = export( (GM_MultiPoint)geom );
    }
    else if( geom instanceof GM_MultiCurve )
    {
      pg = export( (GM_MultiCurve)geom );
    }
    else if( geom instanceof GM_MultiSurface )
    {
      pg = export( (GM_MultiSurface)geom );
    }

    Debug.debugMethodEnd();
    return pg;
  }

  /**
   * creates a deegree point from a PostGIS point
   * 
   * @param ppoint
   *          postgis point
   */
  private static GM_Point createPoint( Point ppoint, CS_CoordinateSystem crs ) throws GM_Exception
  {

    // if geometry is 2-dimensional
    GM_Position p = null;
    if( ppoint.getDimension() == 2 )
    {
      // convert PostGIS Point to a GM_Point using the GeometryFactory
      p = GeometryFactory.createGM_Position( new double[]
      { ppoint.getX(), ppoint.getY() } );
      // if geometry is 3-dimensional
    }
    else if( ppoint.getDimension() == 3 )
    {
      // convert PostGIS Point to a GM_Point using the GeometryFactory
      p = GeometryFactory.createGM_Position( new double[]
      { ppoint.getX(), ppoint.getY(), ppoint.getZ() } );
    }

    return GeometryFactory.createGM_Point( p, crs );
  }

  /**
   * creates a deegree curve from a PostGIS linestring
   * 
   * @param lineString
   *          postgis linestring
   */
  private static GM_Curve createCurve( LineString lineString, CS_CoordinateSystem crs )
      throws GM_Exception
  {

    // create a GM_Position Array. Used to store the Points the
    // Curve will consist of
    GM_Position[] points = new GM_Position[lineString.numPoints()];

    // if geometry is 2-dimensional
    if( lineString.getDimension() == 2 )
    {
      // for all Points
      for( int i = 0; i < lineString.numPoints(); i++ )
      {
        // create a GM_Position from the PostGIS Point using the
        // GeometryFactory
        double[] d = new double[]
        { lineString.getPoint( i ).getX(), lineString.getPoint( i ).getY() };
        points[i] = GeometryFactory.createGM_Position( d );
      }
      // if geometry is 3-dimensional
    }
    else if( lineString.getDimension() == 3 )
    {
      // for all Points
      for( int i = 0; i < lineString.numPoints(); i++ )
      {
        // create a GM_Position from the PostGIS Point using the
        // GeometryFactory
        double[] d = new double[]
        { lineString.getPoint( i ).getX(), lineString.getPoint( i ).getY(),
            lineString.getPoint( i ).getZ() };
        points[i] = GeometryFactory.createGM_Position( d );
      }
    }

    return GeometryFactory.createGM_Curve( points, crs );
  }

  /**
   * creates a deegree surface from a PostGIS polygon
   * 
   * @param polygon
   *          postgis polygon
   */
  private static GM_Surface createSurface( Polygon polygon, CS_CoordinateSystem crs )
      throws GM_Exception
  {

    // create a GM_Position Array. Used to store the Positions the
    // exterior Ring of the Surface will consist of
    GM_Position[] eRing = new GM_Position[polygon.getRing( 0 ).numPoints()];
    // declares a GM_Position[][] Array. Used to store the Positions
    // of the interior Rings the Surface will consist of. The exterior
    // Ring is stored seperately
    GM_Position[][] iRings = null;

    // if geometry is 2-dimensional
    if( polygon.getDimension() == 2 )
    {
      // for all the Points of the fist LinearRing (which is the exterior)
      LinearRing ring = polygon.getRing( 0 );
      for( int j = 0; j < eRing.length; j++ )
      {
        // store all the Points of the exterior Ring in the Array
        // eRing. Convert them using GeometryFactory
        double[] d = new double[]
        { ring.getPoint( j ).getX(), ring.getPoint( j ).getY() };
        eRing[j] = GeometryFactory.createGM_Position( d );
      }

      if( polygon.numRings() > 1 )
      {
        iRings = new GM_Position[polygon.numRings() - 1][];
        // for all LinearRings except the first one (which is the exterior one)
        for( int i = 1; i < polygon.numRings(); i++ )
        {
          iRings[i - 1] = new GM_Position[polygon.getRing( i ).numPoints()];
          // for all the Points in the ith LinearRing
          ring = polygon.getRing( i );
          for( int j = 0; j < ring.numPoints(); j++ )
          {
            // store all the Points of the ith interior Ring in
            // the iRings Array
            double[] d = new double[]
            { ring.getPoint( j ).getX(), ring.getPoint( j ).getY() };
            iRings[i - 1][j] = GeometryFactory.createGM_Position( d );
          }
        }
      }
      // if geometry is 3-dimensional
    }
    else if( polygon.getDimension() == 3 )
    {
      // for all the Points of the fist LinearRing (which is the exterior)
      LinearRing ring = polygon.getRing( 0 );
      for( int j = 0; j < ring.numPoints(); j++ )
      {
        // store all the Points of the exterior Ring in the Array
        // eRing. Convert them using GeometryFactory
        double[] d = new double[]
        { ring.getPoint( j ).getX(), ring.getPoint( j ).getY(), ring.getPoint( j ).getZ() };
        eRing[j] = GeometryFactory.createGM_Position( d );
      }

      if( polygon.numRings() > 1 )
      {
        iRings = new GM_Position[polygon.numRings() - 1][];
        // for all LinearRings except the first one (which is the exterior one)
        for( int i = 1; i < polygon.numRings(); i++ )
        {
          iRings[i - 1] = new GM_Position[polygon.getRing( i ).numPoints()];
          // for all the Points in the ith LinearRing
          ring = polygon.getRing( i );
          for( int j = 0; j < ring.numPoints(); j++ )
          {
            // store all the Points of the ith interior Ring in the iRings Array
            double[] d = new double[]
            { ring.getPoint( j ).getX(), ring.getPoint( j ).getY(), ring.getPoint( j ).getZ() };
            iRings[i][j] = GeometryFactory.createGM_Position( d );
          }
        }
      }
    }

    return GeometryFactory
        .createGM_Surface( eRing, iRings, new GM_SurfaceInterpolation_Impl(), crs );
  }

  /**
   * creates a deegree multi point from a PostGIS multi point
   * 
   * @param mpoint
   *          postgis multi point
   */
  private static GM_MultiPoint createMultiPoint( MultiPoint mpoint, CS_CoordinateSystem crs )
      throws GM_Exception
  {

    // create a temporary GM_Point Array to store the Points the
    // MultiPoint will consist of
    GM_Point[] mpoints = new GM_Point[mpoint.numPoints()];

    // for all Points
    for( int i = 0; i < mpoint.numPoints(); i++ )
    {
      // convert PostGIS Point to a GM_Point using the GeometryFactory
      mpoints[i] = createPoint( mpoint.getPoint( i ), crs );
    }

    // create a GM_Multipoint from the Array points
    return GeometryFactory.createGM_MultiPoint( mpoints );

  }

  /**
   * creates a deegree multi curve from a PostGIS multi lineString
   * 
   * @param mlinestring
   *          postgis multi lineString
   */
  private static GM_MultiCurve createMultiCurve( MultiLineString mlinestring,
      CS_CoordinateSystem crs ) throws GM_Exception
  {
    // create a GM_Curve Array. Used to store the CurveSegments the
    // Curve will consist of
    GM_Curve[] curves = new GM_Curve[mlinestring.numLines()];

    // for all Lines
    for( int i = 0; i < mlinestring.numLines(); i++ )
    {
      // create a GM_Curve form the positions Array using the
      // GeometryFactory
      curves[i] = createCurve( mlinestring.getLine( i ), crs );
    }

    // create a GM_Curve form all the CurveSegments stored in the
    // csegments Array using the GeometryFactory
    return GeometryFactory.createGM_MultiCurve( curves );
  }

  /**
   * creates a deegree multi surface from a PostGIS multi polygon
   * 
   * @param mpolygon
   *          postgis multi polygon
   */
  private static GM_MultiSurface createMultiSurface( MultiPolygon mpolygon, CS_CoordinateSystem crs )
      throws GM_Exception
  {

    // create a GM_Surfaces Array. Used to store the Surfaces the
    // MultiSurface will consist of
    GM_Surface[] surfaces = new GM_Surface[mpolygon.numPolygons()];

    // for all Polygons the MultiPolygon consists of
    for( int i = 0; i < mpolygon.numPolygons(); i++ )
    {
      surfaces[i] = createSurface( mpolygon.getPolygon( i ), crs );
    }

    return GeometryFactory.createGM_MultiSurface( surfaces );
  }

  /**
   * transforms the passed deegree GM_Point to a postgis point
   * 
   * @param point
   *          deegree GM_Point
   */
  private static Point export( GM_Point point ) throws GM_Exception
  {
    Debug.debugMethodBegin( "PostGISAdapter", "export(GM_Point)" );

    StringBuffer sb = WKTAdapter.export( point );
    Point pgPoint = null;

    try
    {
      pgPoint = new Point( sb.toString() );
    }
    catch( Exception e )
    {
      throw new GM_Exception( e.toString() );
    }

    Debug.debugMethodEnd();
    return pgPoint;
  }

  /**
   * transforms the passed deegree GM_Curve to a postgis LineString
   * 
   * @param curve
   *          deegree GM_Curve
   */
  private static LineString export( GM_Curve curve ) throws GM_Exception
  {
    Debug.debugMethodBegin( "PostGISAdapter", "export(GM_Curve)" );

    StringBuffer sb = WKTAdapter.export( curve );
    LineString pgLineString = null;

    try
    {
      pgLineString = new LineString( sb.toString() );
    }
    catch( Exception e )
    {
      throw new GM_Exception( e.toString() );
    }

    Debug.debugMethodEnd();
    return pgLineString;
  }

  /**
   * transforms the passed deegree GM_Surface to a postgis Polygon
   * 
   * @param surface
   *          deegree GM_Surface
   */
  private static Polygon export( GM_Surface surface ) throws GM_Exception
  {
    Debug.debugMethodBegin( "PostGISAdapter", "export(GM_Surface)" );

    StringBuffer sb = WKTAdapter.export( surface );
    Polygon pgPoly = null;

    try
    {
      pgPoly = new Polygon( sb.toString() );
    }
    catch( Exception e )
    {
      throw new GM_Exception( e.toString() );
    }

    Debug.debugMethodEnd();
    return pgPoly;
  }

  /**
   * transforms the passed deegree GM_MultiPoint to a postgis multipoint
   * 
   * @param mpoint
   *          deegree GM_MultiPoint
   */
  private static MultiPoint export( GM_MultiPoint mpoint ) throws GM_Exception
  {
    Debug.debugMethodBegin( "PostGISAdapter", "export(GM_MultiPoint)" );

    StringBuffer sb = WKTAdapter.export( mpoint );
    MultiPoint pgMPoint = null;

    try
    {
      pgMPoint = new MultiPoint( sb.toString() );
    }
    catch( Exception e )
    {
      throw new GM_Exception( e.toString() );
    }

    Debug.debugMethodEnd();
    return pgMPoint;
  }

  /**
   * transforms the passed deegree GM_MultiCurve to a postgis MultiLineString
   * 
   * @param mcurve
   *          deegree GM_MultiCurve
   */
  private static MultiLineString export( GM_MultiCurve mcurve ) throws GM_Exception
  {
    Debug.debugMethodBegin( "PostGISAdapter", "export(GM_MultiCurve)" );

    StringBuffer sb = WKTAdapter.export( mcurve );
    MultiLineString pgMLineString = null;

    try
    {
      pgMLineString = new MultiLineString( sb.toString() );
    }
    catch( Exception e )
    {
      throw new GM_Exception( e.toString() );
    }

    Debug.debugMethodEnd();
    return pgMLineString;
  }

  /**
   * transforms the passed deegree GM_MultiSurface to a postgis MultiPolygon
   * 
   * @param msurface
   *          deegree GM_MultiSurface
   */
  private static MultiPolygon export( GM_MultiSurface msurface ) throws GM_Exception
  {
    Debug.debugMethodBegin( "PostGISAdapter", "export(GM_MultiSurface)" );

    StringBuffer sb = WKTAdapter.export( msurface );
    MultiPolygon pgMPoly = null;

    try
    {
      pgMPoly = new MultiPolygon( sb.toString() );
    }
    catch( Exception e )
    {
      throw new GM_Exception( e.toString() );
    }

    Debug.debugMethodEnd();
    return pgMPoly;
  }

}