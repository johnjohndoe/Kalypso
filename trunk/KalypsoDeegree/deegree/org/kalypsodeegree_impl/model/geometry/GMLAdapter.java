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

import java.io.StringReader;
import java.rmi.RemoteException;

import org.deegree.gml.GMLBox;
import org.deegree.gml.GMLCoord;
import org.deegree.gml.GMLCoordinates;
import org.deegree.gml.GMLGeometry;
import org.deegree.gml.GMLLineString;
import org.deegree.gml.GMLLinearRing;
import org.deegree.gml.GMLMultiLineString;
import org.deegree.gml.GMLMultiPoint;
import org.deegree.gml.GMLMultiPolygon;
import org.deegree.gml.GMLPoint;
import org.deegree.gml.GMLPolygon;
import org.deegree.model.geometry.GM_Curve;
import org.deegree.model.geometry.GM_CurveSegment;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree.model.geometry.GM_Exception;
import org.deegree.model.geometry.GM_MultiCurve;
import org.deegree.model.geometry.GM_MultiPoint;
import org.deegree.model.geometry.GM_MultiSurface;
import org.deegree.model.geometry.GM_Object;
import org.deegree.model.geometry.GM_Point;
import org.deegree.model.geometry.GM_Polygon;
import org.deegree.model.geometry.GM_Position;
import org.deegree.model.geometry.GM_Surface;
import org.deegree.model.geometry.GM_SurfaceInterpolation;
import org.deegree.model.geometry.GM_SurfacePatch;
import org.deegree.xml.XMLTools;
import org.deegree_impl.gml.GMLCoordinatesParser_Impl;
import org.deegree_impl.gml.GMLFactory;
import org.deegree_impl.model.cs.Adapters;
import org.deegree_impl.model.cs.ConvenienceCSFactory;
import org.deegree_impl.model.cs.CoordinateSystem;
import org.deegree_impl.tools.Debug;
import org.opengis.cs.CS_CoordinateSystem;
import org.w3c.dom.Document;

/**
 * 
 * 
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 */
public class GMLAdapter
{

  /**
   * creates FeatureCollections from OGC WKBs
   *  
   */
  public static String export( GM_Object gmObject ) throws GM_Exception
  {
    Debug.debugMethodBegin();

    StringBuffer sb = null;

    // create geometries from the wkb considering the geomerty typ
    try
    {

      if( gmObject instanceof GM_Point )
      {
        sb = createPoint( (GM_Point)gmObject );
      }
      else if( gmObject instanceof GM_Curve )
      {
        sb = createCurve( (GM_Curve)gmObject );
      }
      else if( gmObject instanceof GM_Surface )
      {
        sb = createSurface( (GM_Surface)gmObject );
      }
      else if( gmObject instanceof GM_MultiPoint )
      {
        sb = createMultiPoint( (GM_MultiPoint)gmObject );
      }
      else if( gmObject instanceof GM_MultiCurve )
      {
        sb = createMultiCurve( (GM_MultiCurve)gmObject );
      }
      else if( gmObject instanceof GM_MultiSurface )
      {
        sb = createMultiSurface( (GM_MultiSurface)gmObject );
      }
    }
    catch( RemoteException e )
    {
      throw new GM_Exception( e.toString() );
    }

    Debug.debugMethodEnd();
    return sb.toString();
  }

  public static String export( GM_Envelope envelope ) throws GM_Exception
  {
    Debug.debugMethodBegin();
    StringBuffer sb = new StringBuffer( "<gml:Box xmlns:gml='http://www.opengis.net/gml'>" );
    sb.append( "<gml:coord><gml:X>" );
    sb.append( envelope.getMin().getX() );
    sb.append( "</gml:X><gml:Y>" );
    sb.append( envelope.getMin().getY() );
    sb.append( "</gml:Y></gml:coord><gml:coord><gml:X>" );
    sb.append( envelope.getMax().getX() );
    sb.append( "</gml:X><gml:Y>" );
    sb.append( envelope.getMax().getY() );
    sb.append( "</gml:Y></gml:coord></gml:Box>" );
    Debug.debugMethodEnd();
    return sb.toString();
  }

  /**
   * Converts a GML geometry object to a corresponding <tt>GM_Object</tt>.
   * <p>
   * Currently, the following conversions are supported:
   * <ul>
   * <li>GMLPoint -> GM_Point
   * <li>GMLMultiPoint -> GM_MultiPoint
   * <li>GMLLineString -> GM_Curve
   * <li>GMLMultiLineString -> GM_MultiCurve
   * <li>GMLPolygon -> GM_Surface
   * <li>GMLMultiPolygon -> GM_MultiSurface
   * </ul>
   * <p>
   * 
   * @return the corresponding <tt>GM_Object</tt>
   * @throws GM_Exception
   *           if type unsupported or conversion failed
   */
  public static GM_Object wrap( String gml ) throws GM_Exception
  {
    Debug.debugMethodBegin();

    GMLGeometry gmlGeo = null;
    try
    {
      StringReader reader = new StringReader( gml );
      Document doc = XMLTools.parse( reader );
      gmlGeo = GMLFactory.createGMLGeometry( doc.getDocumentElement() );
    }
    catch( Exception e )
    {
      throw new GM_Exception( e.toString() );
    }

    GM_Object geo = wrap( gmlGeo );

    Debug.debugMethodEnd();
    return geo;
  }

  /**
   * Converts a GML geometry object to a corresponding <tt>GM_Object</tt>.
   * <p>
   * Currently, the following conversions are supported:
   * <ul>
   * <li>GMLPoint -> GM_Point
   * <li>GMLMultiPoint -> GM_MultiPoint
   * <li>GMLLineString -> GM_Curve
   * <li>GMLMultiLineString -> GM_MultiCurve
   * <li>GMLPolygon -> GM_Surface
   * <li>GMLMultiPolygon -> GM_MultiSurface
   * <li>GMLBox -> GM_Surface
   * </ul>
   * <p>
   * 
   * @return the corresponding <tt>GM_Object</tt>
   * @throws GM_Exception
   *           if type unsupported or conversion failed
   */
  public static GM_Object wrap( GMLGeometry gml ) throws GM_Exception
  {
    Debug.debugMethodBegin();

    GM_Object geo = null;

    if( gml instanceof GMLBox )
    {
      geo = wrap( (GMLBox)gml );
    }
    else if( gml instanceof GMLPoint )
    {
      geo = wrap( (GMLPoint)gml );
    }
    else if( gml instanceof GMLLineString )
    {
      geo = wrap( (GMLLineString)gml );
    }
    else if( gml instanceof GMLPolygon )
    {
      geo = wrap( (GMLPolygon)gml );
    }
    else if( gml instanceof GMLMultiPoint )
    {
      geo = wrap( (GMLMultiPoint)gml );
    }
    else if( gml instanceof GMLMultiLineString )
    {
      geo = wrap( (GMLMultiLineString)gml );
    }
    else if( gml instanceof GMLMultiPolygon )
    {
      geo = wrap( (GMLMultiPolygon)gml );
    }

    Debug.debugMethodEnd();
    return geo;
  }

  /**
   * creates a GML expression of a point geometry
   * 
   * @param o
   *          point geometry
   * 
   * @return
   */
  private static StringBuffer createPoint( GM_Point o ) throws RemoteException
  {
    Debug.debugMethodBegin();

    StringBuffer sb = new StringBuffer();

    String crs = null;
    if( o.getCoordinateSystem() != null )
    {
      crs = o.getCoordinateSystem().getName().replace( ' ', ':' );
    }

    if( crs != null )
    {
      sb.append( "<gml:Point srsName=\"" + crs + "\">" );
    }
    else
    {
      sb.append( "<gml:Point>" );
    }

    sb.append( "<gml:coordinates cs=\",\" decimal=\".\" ts=\" \">" );
    sb.append( o.getX() + "," + o.getY() );
    if( o.getCoordinateDimension() == 3 )
    {
      sb.append( "," + o.getZ() );
    }
    sb.append( "</gml:coordinates>" );
    sb.append( "</gml:Point>" );

    Debug.debugMethodEnd();

    return sb;
  }

  /**
   * creates a GML expression of a curve geometry
   * 
   * @param o
   *          curve geometry
   * 
   * @return @throws
   *         GM_Exception
   */
  private static StringBuffer createCurve( GM_Curve o ) throws RemoteException, GM_Exception
  {
    Debug.debugMethodBegin( "GMLAdapter", "createCurves" );

    GM_Position[] p = o.getAsLineString().getPositions();

    StringBuffer sb = new StringBuffer( p.length * 40 );

    String crs = null;
    if( o.getCoordinateSystem() != null )
    {
      crs = o.getCoordinateSystem().getName().replace( ' ', ':' );
    }

    if( crs != null )
    {
      sb.append( "<gml:LineString srsName=\"" + crs + "\">" );
    }
    else
    {
      sb.append( "<gml:LineString>" );
    }

    sb.append( "<gml:coordinates cs=\",\" decimal=\".\" ts=\" \">" );

    for( int i = 0; i < ( p.length - 1 ); i++ )
    {
      sb.append( p[i].getX() + "," + p[i].getY() );
      if( o.getCoordinateDimension() == 3 )
      {
        sb.append( "," + p[i].getZ() + " " );
      }
      else
      {
        sb.append( " " );
      }
    }

    sb.append( p[p.length - 1].getX() + "," + p[p.length - 1].getY() );
    if( o.getCoordinateDimension() == 3 )
    {
      sb.append( "," + p[p.length - 1].getZ() );
    }
    sb.append( "</gml:coordinates>" );
    sb.append( "</gml:LineString>" );

    Debug.debugMethodEnd();

    return sb;
  }

  /**
   * @param sur
   * @return @throws
   *         RemoteException
   * @throws GM_Exception
   */
  private static StringBuffer createSurface( GM_Surface sur ) throws RemoteException, GM_Exception
  {
    Debug.debugMethodBegin( "GMLAdapter", "createSurfaces" );

    StringBuffer sb = new StringBuffer( 5000 );

    String crs = null;
    if( sur.getCoordinateSystem() != null )
    {
      crs = sur.getCoordinateSystem().getName().replace( ' ', ':' );
    }

    if( crs != null )
    {
      sb.append( "<gml:Polygon srsName=\"" + crs + "\">" );
    }
    else
    {
      sb.append( "<gml:Polygon>" );
    }

    GM_SurfacePatch patch = sur.getSurfacePatchAt( 0 );

    // exterior ring
    sb.append( "<gml:outerBoundaryIs><gml:LinearRing>" );
    sb.append( "<gml:coordinates cs=\",\" decimal=\".\" ts=\" \">" );

    GM_Position[] p = patch.getExteriorRing();

    for( int i = 0; i < ( p.length - 1 ); i++ )
    {
      sb.append( p[i].getX() + "," + p[i].getY() );
      if( sur.getCoordinateDimension() == 3 )
      {
        sb.append( "," + p[i].getZ() + " " );
      }
      else
      {
        sb.append( " " );
      }
    }

    sb.append( p[p.length - 1].getX() + "," + p[p.length - 1].getY() );
    if( sur.getCoordinateDimension() == 3 )
    {
      sb.append( "," + p[p.length - 1].getZ() );
    }
    sb.append( "</gml:coordinates>" );
    sb.append( "</gml:LinearRing></gml:outerBoundaryIs>" );

    // interior rings
    GM_Position[][] ip = patch.getInteriorRings();

    if( ip != null )
    {
      for( int j = 0; j < ip.length; j++ )
      {
        sb.append( "<gml:innerBoundaryIs><gml:LinearRing>" );
        sb.append( "<gml:coordinates cs=\",\" decimal=\".\" ts=\" \">" );

        for( int i = 0; i < ( ip[j].length - 1 ); i++ )
        {
          sb.append( ip[j][i].getX() + "," + ip[j][i].getY() );
          if( sur.getCoordinateDimension() == 3 )
          {
            sb.append( "," + ip[j][i].getZ() + " " );
          }
          else
          {
            sb.append( " " );
          }
        }

        sb.append( ip[j][ip[j].length - 1].getX() + "," + ip[j][ip[j].length - 1].getY() );
        if( sur.getCoordinateDimension() == 3 )
        {
          sb.append( "," + ip[j][ip[j].length - 1].getZ() );
        }
        sb.append( "</gml:coordinates>" );
        sb.append( "</gml:LinearRing></gml:innerBoundaryIs>" );
      }
    }

    sb.append( "</gml:Polygon>" );

    Debug.debugMethodEnd();

    return sb;
  }

  /**
   * @param mp
   * @return @throws
   *         RemoteException
   */
  private static StringBuffer createMultiPoint( GM_MultiPoint mp ) throws RemoteException
  {
    Debug.debugMethodBegin( "GMLAdapter", "createMultiPoints" );

    StringBuffer sb = new StringBuffer( mp.getSize() * 35 );
    String srsName = "";

    String crs = null;
    if( mp.getCoordinateSystem() != null )
    {
      crs = mp.getCoordinateSystem().getName().replace( ' ', ':' );
    }

    if( crs != null )
    {
      srsName = " srsName=\"" + crs + "\"";
    }

    sb.append( "<gml:MultiPoint" ).append( srsName ).append( ">" );

    for( int i = 0; i < mp.getSize(); i++ )
    {
      sb.append( "<gml:pointMember>" );
      sb.append( "<gml:Point" ).append( srsName ).append( ">" );

      sb.append( "<gml:coordinates cs=\",\" decimal=\".\" ts=\" \">" );
      sb.append( mp.getPointAt( i ).getX() + "," + mp.getPointAt( i ).getY() );
      if( mp.getPointAt( i ).getCoordinateDimension() == 3 )
      {
        sb.append( "," + mp.getPointAt( i ).getZ() );
      }
      sb.append( "</gml:coordinates>" );
      sb.append( "</gml:Point>" );
      sb.append( "</gml:pointMember>" );
    }

    sb.append( "</gml:MultiPoint>" );

    Debug.debugMethodEnd();

    return sb;
  }

  /**
   * @param mp
   * @return @throws
   *         RemoteException
   * @throws GM_Exception
   */
  private static StringBuffer createMultiCurve( GM_MultiCurve mp ) throws RemoteException,
      GM_Exception
  {
    Debug.debugMethodBegin( "GMLAdapter", "createMultiCurves" );

    StringBuffer sb = new StringBuffer( 10000 );

    String srsName = "";

    String crs = null;
    if( mp.getCoordinateSystem() != null )
    {
      crs = mp.getCoordinateSystem().getName().replace( ' ', ':' );
    }

    if( crs != null )
    {
      srsName = " srsName=\"" + crs + "\"";
    }

    sb.append( "<gml:MultiLineString" ).append( srsName ).append( ">" );

    for( int j = 0; j < mp.getSize(); j++ )
    {
      sb.append( "<gml:lineStringMember>" );
      sb.append( "<gml:LineString" ).append( srsName ).append( ">" );

      sb.append( "<gml:coordinates cs=\",\" decimal=\".\" ts=\" \">" );

      GM_Position[] p = mp.getCurveAt( j ).getAsLineString().getPositions();

      for( int i = 0; i < ( p.length - 1 ); i++ )
      {
        sb.append( p[i].getX() + "," + p[i].getY() );
        if( mp.getCoordinateDimension() == 3 )
        {
          sb.append( "," + p[i].getZ() + " " );
        }
        else
        {
          sb.append( " " );
        }
      }

      sb.append( p[p.length - 1].getX() + "," + p[p.length - 1].getY() );
      if( mp.getCoordinateDimension() == 3 )
      {
        sb.append( "," + p[p.length - 1].getZ() );
      }
      sb.append( "</gml:coordinates>" );
      sb.append( "</gml:LineString>" );
      sb.append( "</gml:lineStringMember>" );
    }

    sb.append( "</gml:MultiLineString>" );

    Debug.debugMethodEnd();

    return sb;
  }

  /**
   * @param mp
   * @return @throws
   *         RemoteException
   * @throws GM_Exception
   */
  private static StringBuffer createMultiSurface( GM_MultiSurface mp ) throws RemoteException,
      GM_Exception
  {
    Debug.debugMethodBegin( "GMLAdapter", "createMultiSurfaces" );

    StringBuffer sb = new StringBuffer( 10000 );

    String srsName = "";

    String crs = null;
    if( mp.getCoordinateSystem() != null )
    {
      crs = mp.getCoordinateSystem().getName().replace( ' ', ':' );
    }

    if( crs != null )
    {
      srsName = " srsName=\"" + crs + "\"";
    }

    sb.append( "<gml:MultiPolygon" ).append( srsName ).append( ">" );

    for( int k = 0; k < mp.getSize(); k++ )
    {
      sb.append( "<gml:polygonMember>" );
      sb.append( "<gml:Polygon" ).append( srsName ).append( ">" );

      GM_Surface sur = mp.getSurfaceAt( k );
      GM_SurfacePatch patch = sur.getSurfacePatchAt( 0 );

      // exterior ring
      sb.append( "<gml:outerBoundaryIs><gml:LinearRing>" );
      sb.append( "<gml:coordinates cs=\",\" decimal=\".\" ts=\" \">" );

      GM_Position[] p = patch.getExteriorRing();

      for( int i = 0; i < ( p.length - 1 ); i++ )
      {
        sb.append( p[i].getX() + "," + p[i].getY() );
        if( mp.getCoordinateDimension() == 3 )
        {
          sb.append( "," + p[i].getZ() + " " );
        }
        else
        {
          sb.append( " " );
        }
      }

      sb.append( p[p.length - 1].getX() + "," + p[p.length - 1].getY() );
      if( mp.getCoordinateDimension() == 3 )
      {
        sb.append( "," + p[p.length - 1].getZ() );
      }
      sb.append( "</gml:coordinates>" );
      sb.append( "</gml:LinearRing></gml:outerBoundaryIs>" );

      // interior rings
      GM_Position[][] ip = patch.getInteriorRings();

      if( ip != null )
      {
        for( int j = 0; j < ip.length; j++ )
        {
          sb.append( "<gml:innerBoundaryIs><gml:LinearRing>" );
          sb.append( "<gml:coordinates cs=\",\" decimal=\".\" ts=\" \">" );

          for( int i = 0; i < ( ip[j].length - 1 ); i++ )
          {
            sb.append( ip[j][i].getX() + "," + ip[j][i].getY() );
            if( mp.getCoordinateDimension() == 3 )
            {
              sb.append( "," + ip[j][i].getZ() + " " );
            }
            else
            {
              sb.append( " " );
            }
          }

          sb.append( ip[j][ip[j].length - 1].getX() + "," + ip[j][ip[j].length - 1].getY() );
          if( mp.getCoordinateDimension() == 3 )
          {
            sb.append( "," + ip[j][ip[j].length - 1].getZ() );
          }
          sb.append( "</gml:coordinates>" );
          sb.append( "</gml:LinearRing></gml:innerBoundaryIs>" );
        }
      }

      sb.append( "</gml:Polygon>" );
      sb.append( "</gml:polygonMember>" );
    }

    sb.append( "</gml:MultiPolygon>" );

    Debug.debugMethodEnd();

    return sb;
  }

  /**
   * creates a GM_Point from a gml.
   * 
   * @param gml
   *          a GMLPoint
   */
  private static GM_Point wrap( GMLPoint gml ) throws GM_Exception
  {
    Debug.debugMethodBegin( "GMLAdapter", "wrap(GMLPoint)" );

    CS_CoordinateSystem crs = null;
    GM_Position[] pointarray = null;
    GM_Point point = null;

    String srs = gml.getSrs();

    if( srs != null )
    {
      ConvenienceCSFactory csFac = ConvenienceCSFactory.getInstance();
      CoordinateSystem cs = csFac.getCSByName( srs );
      Adapters adapters = Adapters.getDefault();
      crs = adapters.export( cs );
    }

    GMLCoordinates s = gml.getCoordinates();
    if( s != null )
    {
      pointarray = GMLCoordinatesParser_Impl.coordinatesToPoints( s );
      point = new GM_Point_Impl( pointarray[0], crs );
    }
    else
    {
      GMLCoord coord = gml.getCoord();
      point = new GM_Point_Impl( coord.getX(), coord.getY(), coord.getZ(), crs );
    }

    Debug.debugMethodEnd();
    return point;
  }

  /**
   * creates a GM_CurveSegment from an array of GML-points.
   * 
   * @param linestring
   *          a GMLLineString
   */
  private static GM_Curve wrap( GMLLineString linestring ) throws GM_Exception
  {
    Debug.debugMethodBegin();

    GM_Curve curve = null;
    CS_CoordinateSystem crs = null;
    GM_Position[] pointarray = null;

    String srs = linestring.getSrs();

    if( srs != null )
    {
      ConvenienceCSFactory csFac = ConvenienceCSFactory.getInstance();
      CoordinateSystem cs = csFac.getCSByName( srs );
      Adapters adapters = Adapters.getDefault();
      crs = adapters.export( cs );
    }

    GMLCoordinates s = linestring.getCoordinates();

    if( s != null )
    {
      pointarray = GMLCoordinatesParser_Impl.coordinatesToPoints( s );
    }
    else
    {
      GMLCoord[] coord = linestring.getCoords();
      pointarray = new GM_Position[coord.length];

      for( int i = 0; i < coord.length; i++ )
      {
        pointarray[i] = new GM_Position_Impl( coord[i].getX(), coord[i].getY() );
      }
    }

    GM_CurveSegment[] curvesegment = new GM_CurveSegment[1];
    curvesegment[0] = GeometryFactory.createGM_CurveSegment( pointarray, crs );
    curve = GeometryFactory.createGM_Curve( curvesegment );

    Debug.debugMethodEnd();
    return curve;
  }

  /**
   * creates a GM_Surface
   * 
   * @param polygon
   *          GMLPolygon
   */
  private static GM_Surface wrap( GMLPolygon polygon ) throws GM_Exception
  {
    Debug.debugMethodBegin( "GMLAdapter", "wrap(GMLPolygon)" );

    GM_Position[] ext = null;
    GM_Position[][] inner = null;
    GMLCoordinates[] intcoordinates = null;
    CS_CoordinateSystem crs = null;

    String srs = polygon.getSrs();

    if( srs != null )
    {
      ConvenienceCSFactory csFac = ConvenienceCSFactory.getInstance();
      CoordinateSystem cs = csFac.getCSByName( srs );
      Adapters adapters = Adapters.getDefault();
      crs = adapters.export( cs );
    }

    // reads the coordinates from the exterior Ring of the Polygon
    GMLCoordinates extcoordinates = polygon.getExteriorRing().getCoordinates();

    if( extcoordinates != null )
    {
      // Do it with GMLCoordinates
      // creates a GM_Point-Array, to save the Coordinates and give them
      // to the Constructor of the GM_LineString
      ext = GMLCoordinatesParser_Impl.coordinatesToPoints( extcoordinates );

      // the linestring-array with the length of the number of inner polygons.
      GMLLinearRing[] lr = polygon.getInteriorRings();

      if( lr != null )
      {
        inner = new GM_Position[lr.length][];
        intcoordinates = new GMLCoordinates[inner.length];

        for( int i = 0; i < inner.length; i++ )
        {
          intcoordinates[i] = lr[i].getCoordinates();
          inner[i] = GMLCoordinatesParser_Impl.coordinatesToPoints( intcoordinates[i] );
        }
      }
    }
    else
    {
      // The GMLChoord-array gets filled with Coords
      GMLCoord[] coords = polygon.getExteriorRing().getCoord();

      // The size of the array of GM_Position.
      ext = new GM_Position[coords.length];

      // Exterior Ring
      for( int i = 0; i < coords.length; i++ )
      {
        ext[i] = new GM_Position_Impl( coords[i].getX(), coords[i].getY() );
      }

      GMLLinearRing[] glr = polygon.getInteriorRings();

      if( glr != null )
      {
        inner = new GM_Position[glr.length][];

        // Interior Rings
        for( int i = 0; i < inner.length; i++ )
        {
          GMLLinearRing lr = glr[i];
          coords = lr.getCoord();

          // The size of the array of GM_Points.
          inner[i] = new GM_Position[coords.length];

          // Exterior Ring
          for( int j = 0; j < coords.length; j++ )
          {
            inner[i][j] = new GM_Position_Impl( coords[j].getX(), coords[j].getY() );
          }
        }
      }
    }

    // creates a GM_Polygon
    GM_SurfaceInterpolation si = new GM_SurfaceInterpolation_Impl();
    GM_Polygon gmpol = new GM_Polygon_Impl( si, ext, inner, crs );
    GM_Surface surface = new GM_Surface_Impl( gmpol );

    Debug.debugMethodEnd();
    return surface;
  }

  /**
   * creates a GM_MultiPoint from a GMLMultiPoint
   * 
   * @param multipoint
   *          GMLMultiPoint
   */
  private static GM_MultiPoint wrap( GMLMultiPoint multipoint ) throws GM_Exception
  {
    Debug.debugMethodBegin( "GMLAdapter", "wrap(GMLMultiPoint)" );

    GM_MultiPoint gmmp = null;
    GMLPoint[] points = multipoint.getPoints();
    GM_Point[] point = new GM_Point[points.length];
    for( int i = 0; i < points.length; i++ )
    {
      point[i] = wrap( points[i] );
    }

    if( points.length > 0 )
    {
      gmmp = new GM_MultiPoint_Impl( point, point[0].getCoordinateSystem() );
    }
    else
    {
      gmmp = new GM_MultiPoint_Impl( point );
    }

    Debug.debugMethodEnd();
    return gmmp;
  }

  /**
   * creates a GM_MultiCurve from an GMLMultiLinestring.
   * 
   * @param multilinestring
   *          a GMLMultiLineString
   */
  private static GM_MultiCurve wrap( GMLMultiLineString multilinestring ) throws GM_Exception
  {
    Debug.debugMethodBegin( "GMLAdapter", "wrap(GMLMultiLineString)" );

    GM_MultiCurve multicurve = null;
    GMLLineString[] lsarray = multilinestring.getLineStrings();
    GM_Curve[] curvearray = new GM_Curve[lsarray.length];

    for( int i = 0; i < lsarray.length; i++ )
    {
      curvearray[i] = wrap( lsarray[i] );
    }

    if( curvearray.length > 0 )
    {
      multicurve = new GM_MultiCurve_Impl( curvearray, curvearray[0].getCoordinateSystem() );
    }
    else
    {
      multicurve = new GM_MultiCurve_Impl( curvearray );
    }

    Debug.debugMethodEnd();
    return multicurve;
  }

  /**
   * creates a GM_MultiSurface
   * 
   * @param multipolygon
   */
  private static GM_MultiSurface wrap( GMLMultiPolygon multipolygon ) throws GM_Exception
  {
    Debug.debugMethodBegin( "", "createGM_MultiSurface()" );

    GMLPolygon[] polygonarray = multipolygon.getPolygons();

    GM_Surface[] surfaces = new GM_Surface[polygonarray.length];

    for( int i = 0; i < polygonarray.length; i++ )
    {
      surfaces[i] = wrap( polygonarray[i] );
    }

    GM_MultiSurface multisurface = null;
    if( surfaces.length > 0 )
    {
      multisurface = new GM_MultiSurface_Impl( surfaces, surfaces[0].getCoordinateSystem() );
    }
    else
    {
      multisurface = new GM_MultiSurface_Impl( surfaces );
    }
    Debug.debugMethodEnd();
    return multisurface;
  }

  /**
   * creates a GM_Envelope object out from a GMLBox
   */
  public static GM_Envelope createGM_Envelope( GMLBox box )
  {
    double x = box.getMin().getX();
    double y = box.getMin().getY();
    GM_Position min = GeometryFactory.createGM_Position( x, y );
    x = box.getMax().getX();
    y = box.getMax().getY();
    GM_Position max = GeometryFactory.createGM_Position( x, y );
    return new GM_Envelope_Impl( min, max );
  }

  /**
   * @param gmlBox
   * @return @throws
   *         GM_Exception
   */
  private static GM_Surface wrap( GMLBox gmlBox ) throws GM_Exception
  {
    String srs = gmlBox.getSrs();
    CS_CoordinateSystem crs = null;
    if( srs != null )
    {
      ConvenienceCSFactory fac = ConvenienceCSFactory.getInstance();
      CoordinateSystem cs_ = fac.getCSByName( srs );
      Adapters ada = Adapters.getDefault();
      crs = ada.export( cs_ );
    }
    GM_Envelope box = createGM_Envelope( gmlBox );
    GM_Position max = box.getMax();
    GM_Position min = box.getMin();
    double x1 = min.getX();
    double y1 = min.getY();
    double x2 = max.getX();
    double y2 = max.getY();
    GM_Position[] exteriorRing = new GM_Position[5];
    exteriorRing[0] = GeometryFactory.createGM_Position( x1, y1 );
    exteriorRing[1] = GeometryFactory.createGM_Position( x1, y2 );
    exteriorRing[2] = GeometryFactory.createGM_Position( x2, y2 );
    exteriorRing[3] = GeometryFactory.createGM_Position( x2, y1 );
    exteriorRing[4] = exteriorRing[0];
    return GeometryFactory.createGM_Surface( exteriorRing, null,
        new GM_SurfaceInterpolation_Impl(), crs );
  }
}