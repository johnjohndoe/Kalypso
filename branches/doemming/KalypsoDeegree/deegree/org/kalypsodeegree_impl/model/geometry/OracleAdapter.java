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

import oracle.sdoapi.geom.CoordPoint;
import oracle.sdoapi.geom.CurveString;
import oracle.sdoapi.geom.Geometry;
import oracle.sdoapi.geom.LineString;
import oracle.sdoapi.geom.MultiLineString;
import oracle.sdoapi.geom.MultiPoint;
import oracle.sdoapi.geom.MultiPolygon;
import oracle.sdoapi.geom.Point;
import oracle.sdoapi.geom.Polygon;

import org.deegree.model.geometry.GM_Curve;
import org.deegree.model.geometry.GM_CurveSegment;
import org.deegree.model.geometry.GM_Exception;
import org.deegree.model.geometry.GM_MultiCurve;
import org.deegree.model.geometry.GM_MultiPoint;
import org.deegree.model.geometry.GM_MultiSurface;
import org.deegree.model.geometry.GM_Object;
import org.deegree.model.geometry.GM_Point;
import org.deegree.model.geometry.GM_Position;
import org.deegree.model.geometry.GM_Surface;
import org.deegree.model.geometry.GM_SurfaceInterpolation;
import org.opengis.cs.CS_CoordinateSystem;


/**
 * 
 *
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 */
public class OracleAdapter {
    
    public static Geometry export( GM_Object geom ) {
        throw new NoSuchMethodError();
    }

    /**
     * creates a deegree geometry object from an oracle sdo geometry
     */
    public static GM_Object wrap( Geometry geometry, CS_CoordinateSystem crs ) throws GM_Exception {
        GM_Object geo = null;

        if ( geometry instanceof Point ) {
            geo = wrapPoint( (Point)geometry, crs );
        } else if ( geometry instanceof LineString ) {
            geo = wrapCurve( (LineString)geometry, crs );
        } else if ( geometry instanceof Polygon ) {
            GM_SurfaceInterpolation si = new GM_SurfaceInterpolation_Impl();
            geo = wrapSurface( (Polygon)geometry, crs, si );
        } else if ( geometry instanceof MultiPoint ) {
            geo = wrapMultiPoint( (MultiPoint)geometry, crs );
        } else if ( geometry instanceof MultiLineString ) {
            geo = wrapMultiCurve( (MultiLineString)geometry, crs );
        } else if ( geometry instanceof MultiPolygon ) {
            GM_SurfaceInterpolation si = new GM_SurfaceInterpolation_Impl();
            geo = wrapMultiSurface( (MultiPolygon)geometry, crs, si );
        }

        return geo;
    }

    /**
    * creates a GM_Point from a oracle sdo point.
    */
    public static GM_Point wrapPoint( Point point, CS_CoordinateSystem srs ) throws GM_Exception {
        GM_Point p = null;
        int dim = point.getCoordinateDimension();

        if ( dim == 2 ) {
            p = GeometryFactory.createGM_Point( point.getX(), point.getY(), srs );
        } else {
            double[] d = new double[] { point.getX(), point.getY(), point.getZ() };
            GM_Position pos = GeometryFactory.createGM_Position( d );
            p = GeometryFactory.createGM_Point( pos, srs );
        }

        return p;
    }

    /**
     * creates a GM_Curve from an oracle sdo <tt>LineString</tt>
     *
     * @param lineString oracle sdo <tt>LineString</tt>
     * @param crs coordinate reference system of the curve
     */
    public static GM_Curve wrapCurve( LineString lineString, CS_CoordinateSystem crs )
                       throws GM_Exception {
        GM_Position[] positions = null;
        double[] arr = lineString.getCoordArray();
        int dim = lineString.getDimensionality();

        if ( dim == 2 ) {
            positions = new GM_Position[arr.length / 2];

            int k = 0;

            for ( int i = 0; i < ( positions.length / 2 ); i++ ) {
                positions[i] = GeometryFactory.createGM_Position( arr[k++], arr[k++] );
            }
        } else {
            positions = new GM_Position[arr.length / 3];

            int k = 0;

            for ( int i = 0; i < ( positions.length / 3 ); i++ ) {
                double[] d = new double[] { arr[k++], arr[k++], arr[k++] };
                positions[i] = GeometryFactory.createGM_Position( d );
            }
        }

        GM_CurveSegment seg = GeometryFactory.createGM_CurveSegment( positions, crs );
        return GeometryFactory.createGM_Curve( seg );
    }

    /**
     * creates a GM_Surface from an oracle sdo <tt>Polygon</tt>
     *
     * @param polygon oracle sdo <tt>Polygon</tt>
     * @param crs spatial reference system of the curve
     * @param si GM_SurfaceInterpolation
     */
    public static GM_Surface wrapSurface( Polygon polygon, CS_CoordinateSystem crs, 
                                   GM_SurfaceInterpolation si ) throws GM_Exception {
        GM_Position[] exteriorRing = null;
        GM_Position[][] interiorRings = null;
        int dim = polygon.getDimensionality();
        CurveString ex = polygon.getExteriorRing();
        CurveString[] in = polygon.getInteriorRingArray();

        if ( dim == 2 ) {
            CoordPoint[] cp = ex.getPointArray();

            exteriorRing = new GM_Position[cp.length] ;
            for ( int i = 0; i < cp.length; i++ ) {
                exteriorRing[i] = GeometryFactory.createGM_Position( cp[i].getX(), cp[i].getY() );
            }

            if ( polygon.getNumRings() > 1 ) {
                interiorRings = new GM_Position[in.length][];

                for ( int i = 0; i < in.length; i++ ) {
                    cp = in[i].getPointArray();
                    interiorRings[i] = new GM_Position[cp.length];

                    for ( int j = 0; j < cp.length; j++ ) {
                        interiorRings[i][j] = GeometryFactory.createGM_Position( cp[j].getX(), cp[j].getY() );
                    }
                }
            }
        } else {
            CoordPoint[] cp = ex.getPointArray();

            for ( int i = 0; i < cp.length; i++ ) {
                double[] d = new double[] { cp[i].getX(), cp[i].getY(), cp[i].getZ() };
                exteriorRing[i] = GeometryFactory.createGM_Position( d );
            }

            if ( polygon.getNumRings() > 1 ) {
                interiorRings = new GM_Position[in.length][];

                for ( int i = 0; i < in.length; i++ ) {
                    cp = in[i].getPointArray();
                    interiorRings[i] = new GM_Position[cp.length];

                    for ( int j = 0; j < cp.length; j++ ) {
                        double[] d = new double[] { cp[j].getX(), cp[j].getY(), cp[j].getZ() };
                        interiorRings[i][j] = GeometryFactory.createGM_Position( d );
                    }
                }
            }
        }

        return GeometryFactory.createGM_Surface( exteriorRing, interiorRings, si, crs );
    }

    /**
     * creates a GM_MultiPoint from an oracle sdo <tt>MultiPoint</tt>.
     *
     * @param multiPoint oracle sdo <tt>MultiPoint</tt>
     * @param crs spatial reference system of the curve
     *
     */
    public static GM_MultiPoint wrapMultiPoint( MultiPoint multiPoint, CS_CoordinateSystem crs )
                                 throws GM_Exception {
        Geometry[] geom = multiPoint.getGeometryArray();
        GM_Point[] points = new GM_Point[geom.length];

        for ( int i = 0; i < geom.length; i++ ) {
            points[i] = wrapPoint( (Point)geom[i], crs );
        }

        return GeometryFactory.createGM_MultiPoint( points );
    }

    /**
     * creates a GM_MultiCurve from an oracle sdo <tt>MultiPoint</tt>.
     *
     * @param multiLineString oracle sdo <tt>MultiLineString</tt>
     * @param crs spatial reference system of the multi curve
     *
     */
    public static GM_MultiCurve wrapMultiCurve( MultiLineString multiLineString, CS_CoordinateSystem crs )
                                 throws GM_Exception {
        Geometry[] geom = multiLineString.getGeometryArray();
        GM_Curve[] curves = new GM_Curve[geom.length];

        for ( int i = 0; i < geom.length; i++ ) {
            curves[i] = wrapCurve( (LineString)geom[i], crs );
        }

        return GeometryFactory.createGM_MultiCurve( curves );
    }

    /**
     * creates a GM_MultiSurface from an oracle sdo <tt>MultiPolygon</tt>.
     *
     * @param multiPolygon oracle sdo <tt>MultiPolygon</tt>
     * @param crs spatial reference system of the multi surface
     *
     */
    public static GM_MultiSurface wrapMultiSurface( MultiPolygon multiPolygon, CS_CoordinateSystem crs, 
                                             GM_SurfaceInterpolation si ) throws GM_Exception {
        Geometry[] geom = multiPolygon.getGeometryArray();
        GM_Surface[] polys = new GM_Surface[geom.length];

        for ( int i = 0; i < geom.length; i++ ) {
            polys[i] = wrapSurface( (Polygon)geom[i], crs, si );
        }

        return GeometryFactory.createGM_MultiSurface( polys );
    }
}