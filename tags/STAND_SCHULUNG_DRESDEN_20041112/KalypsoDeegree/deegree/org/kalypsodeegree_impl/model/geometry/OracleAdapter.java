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
 Lesser General public static License for more details.

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
import java.util.List;

import oracle.spatial.geometry.JGeometry;

import org.deegree.model.geometry.GM_Curve;
import org.deegree.model.geometry.GM_Exception;
import org.deegree.model.geometry.GM_MultiCurve;
import org.deegree.model.geometry.GM_MultiPoint;
import org.deegree.model.geometry.GM_MultiSurface;
import org.deegree.model.geometry.GM_Object;
import org.deegree.model.geometry.GM_Point;
import org.deegree.model.geometry.GM_Position;
import org.deegree.model.geometry.GM_Surface;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * 
 * 
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 */
public class OracleAdapter
{

  public static JGeometry export( GM_Object geom )
  {
    throw new NoSuchMethodError();
  }

  /**
   * creates a deegree geometry object from an oracle sdo geometry
   */
  public static GM_Object wrap( JGeometry geometry ) throws GM_Exception
  {

    int srid = geometry.getSRID();
    // TODO create CRS from Oracle SRID
    CS_CoordinateSystem crs = null;

    return wrap( geometry, crs );
  }

  /**
   * creates a deegree geometry object from an oracle sdo geometry
   */
  public static GM_Object wrap( JGeometry geometry, CS_CoordinateSystem crs ) throws GM_Exception
  {
    GM_Object geo = null;

    switch( geometry.getType() )
    {
    case JGeometry.GTYPE_POINT:
      geo = wrapPoint( geometry, crs );
      break;
    case JGeometry.GTYPE_CURVE:
      geo = wrapCurve( geometry, crs );
      break;
    case JGeometry.GTYPE_POLYGON:
      geo = wrapSurface( geometry, crs );
      break;
    case JGeometry.GTYPE_MULTIPOINT:
      geo = wrapMultiPoint( geometry, crs );
      break;
    case JGeometry.GTYPE_MULTICURVE:
      geo = wrapMultiCurve( geometry, crs );
      break;
    case JGeometry.GTYPE_MULTIPOLYGON:
      geo = wrapMultiSurface( geometry, crs );
      break;
    case JGeometry.GTYPE_COLLECTION:
      throw new GM_Exception( "no supported geometry type: collection" );
    }

    return geo;
  }

  /**
   * creates a GM_Point from a oracle sdo point.
   * 
   * @param geometry
   *          oracle geometry containing a point
   * @param crs
   *          desired CRS
   * @return deegree point
   * @throws GM_Exception
   */
  public static GM_Point wrapPoint( JGeometry geometry, CS_CoordinateSystem crs )
      throws GM_Exception
  {
    double[] ord = geometry.getPoint();
    GM_Position pos = GeometryFactory.createGM_Position( ord );
    return GeometryFactory.createGM_Point( pos, crs );
  }

  /**
   * creates a GM_Curve from an oracle sdo curve
   * 
   * @param geometry
   *          oracle geometry containing a curve
   * @param crs
   *          desired CRS
   * @return deegree curve
   * @throws GM_Exception
   */
  public static GM_Curve wrapCurve( JGeometry geometry, CS_CoordinateSystem crs )
      throws GM_Exception
  {
    Object[] ooe = geometry.getOrdinatesOfElements();
    double[] ord = (double[])ooe[0];
    int dim = geometry.getDimensions();
    return GeometryFactory.createGM_Curve( ord, dim, crs );
  }

  /**
   * creates a GM_Surface from an oracle sdo <tt>Polygon</tt>
   * 
   * @param polygon
   *          oracle sdo <tt>Polygon</tt>
   * @param crs
   *          spatial reference system of the curve
   * @param si
   *          GM_SurfaceInterpolation
   */
  public static GM_Surface wrapSurface( JGeometry geometry, CS_CoordinateSystem crs )
      throws GM_Exception
  {
    Object[] ooe = geometry.getOrdinatesOfElements();
    int dim = geometry.getDimensions();
    double[] ext = (double[])ooe[0];
    double[][] in = null;
    if( ooe.length > 1 )
    {
      for( int i = 0; i < ooe.length - 1; i++ )
      {
        in[i] = (double[])ooe[i + 1];
      }
    }
    return GeometryFactory.createGM_Surface( ext, in, dim, crs );
  }

  /**
   * creates a GM_MultiPoint from an oracle sdo multi point
   * 
   * @param geometry
   *          oracle SDO geometry
   * @param crs
   *          spatial reference system of the curve
   * @return deegree geometry
   * @throws GM_Exception
   *  
   */
  public static GM_MultiPoint wrapMultiPoint( JGeometry geometry, CS_CoordinateSystem crs )
      throws GM_Exception
  {
    Object[] ooe = geometry.getOrdinatesOfElements();
    GM_Point[] points = new GM_Point[ooe.length];

    for( int i = 0; i < ooe.length; i++ )
    {
      double[] ord = (double[])ooe[i];
      GM_Position pos = GeometryFactory.createGM_Position( ord );
      points[i] = GeometryFactory.createGM_Point( pos, crs );
    }

    return GeometryFactory.createGM_MultiPoint( points );
  }

  /**
   * creates a GM_MultiCurve from an oracle sdo multi curve
   * 
   * @param crs
   *          spatial reference system of the multi curve
   * @param geometry
   *          oracle SDO geometry
   * @return deegree geometry
   * @throws GM_Exception
   */
  public static GM_MultiCurve wrapMultiCurve( JGeometry geometry, CS_CoordinateSystem crs )
      throws GM_Exception
  {
    Object[] ooe = geometry.getOrdinatesOfElements();
    int dim = geometry.getDimensions();
    GM_Curve[] curves = new GM_Curve[ooe.length];

    for( int i = 0; i < ooe.length; i++ )
    {
      curves[i] = GeometryFactory.createGM_Curve( (double[])ooe[i], dim, crs );
    }

    return GeometryFactory.createGM_MultiCurve( curves );
  }

  /**
   * creates a GM_MultiSurface from an oracle sdo multi polygon
   * 
   * @param crs
   *          spatial reference system of the multi polygon
   * @param geometry
   *          oracle SDO geometry
   * @return deegree geometry
   * @throws GM_Exception
   *  
   */
  public static GM_MultiSurface wrapMultiSurface( JGeometry geometry, CS_CoordinateSystem crs )
      throws GM_Exception
  {
    Object[] ooe = geometry.getOrdinatesOfElements();
    int dim = geometry.getDimensions();
    List list = new ArrayList( 100 );

    int i = 0;
    while( i < ooe.length )
    {
      double[] ext = (double[])ooe[i++];
      GM_Surface surf = GeometryFactory.createGM_Surface( ext, null, dim, crs );
      boolean within = false;
      List temp = new ArrayList( 100 );
      if( i < ooe.length - 1 )
      {
        do
        {
          double[] ord = (double[])ooe[i++];
          double[] pnt = new double[dim];
          for( int j = 0; j < pnt.length; j++ )
          {
            pnt[j] = ord[j];
          }
          GM_Position pos = GeometryFactory.createGM_Position( pnt );
          within = surf.contains( pos );
          if( within )
          {
            temp.add( ord );
          }
        }
        while( within && i < ooe.length );
        i--;
      }
      double[][] in = new double[temp.size()][];
      in = (double[][])temp.toArray( in );
      list.add( GeometryFactory.createGM_Surface( ext, in, dim, crs ) );
    }

    GM_Surface[] polys = new GM_Surface[list.size()];
    polys = (GM_Surface[])list.toArray( polys );

    return GeometryFactory.createGM_MultiSurface( polys );
  }
}