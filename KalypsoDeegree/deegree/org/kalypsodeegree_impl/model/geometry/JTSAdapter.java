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
import org.deegree.model.geometry.GM_LineString;
import org.deegree.model.geometry.GM_MultiCurve;
import org.deegree.model.geometry.GM_MultiPoint;
import org.deegree.model.geometry.GM_MultiPrimitive;
import org.deegree.model.geometry.GM_MultiSurface;
import org.deegree.model.geometry.GM_Object;
import org.deegree.model.geometry.GM_Point;
import org.deegree.model.geometry.GM_Position;
import org.deegree.model.geometry.GM_Surface;
import org.deegree.model.geometry.GM_SurfacePatch;

import com.vividsolutions.jts.geom.Coordinate;
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
 * Adapter between deegree-<tt>GM_Object</tt>s and JTS-<tt>Geometry<tt> objects.
 * <p>
 * Please note that the generated deegree-objects use null as
 * <tt>CS_CoordinateSystem</tt>!
 * <p>
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider</a>
 * @version $Revision$ $Date$
 */
public class JTSAdapter {

    // precision model that is used for all JTS-Geometries
    private static PrecisionModel pm = new PrecisionModel();

    // factory for creating JTS-Geometries
    private static com.vividsolutions.jts.geom.GeometryFactory jtsFactory = 
        new com.vividsolutions.jts.geom.GeometryFactory (pm, 0);    
    
    /**
     * Converts a <tt>GM_Object</tt> to a corresponding JTS-<tt>Geometry</tt>
     * object.
     * <p>
     * Currently, the following conversions are supported:
     * <ul>
     * <li>GM_Point -> Point
     * <li>GM_MultiPoint -> MultiPoint
     * <li>GM_Curve -> LineString
     * <li>GM_MultiCurve ->  MultiLineString
     * <li>GM_Surface -> Polygon
     * <li>GM_MultiSurface -> MultiPolygon
     * <li>GM_MultiPrimitive -> GeometryCollection
     * </ul>
     * <p>
     * @param gmObject the object to be converted
     * @return the corresponding JTS-<tt>Geometry</tt> object
     * @throws GM_Exception if type unsupported or conversion failed
     */
    public static Geometry export (GM_Object gmObject)
        throws GM_Exception {

        Geometry geometry = null;
        if (gmObject instanceof GM_Point) {
            geometry = export ((GM_Point) gmObject);
        } else if (gmObject instanceof GM_MultiPoint) {
            geometry = export ((GM_MultiPoint) gmObject);
        } else if (gmObject instanceof GM_Curve) {
            geometry = export ((GM_Curve) gmObject);
        } else if (gmObject instanceof GM_MultiCurve) {
            geometry = export ((GM_MultiCurve) gmObject);
        } else if (gmObject instanceof GM_Surface) {
            geometry = export ((GM_Surface) gmObject);
        } else if (gmObject instanceof GM_MultiSurface) {
            geometry = export ((GM_MultiSurface) gmObject);
        } else if (gmObject instanceof GM_MultiPrimitive) {
            geometry = export ((GM_MultiPrimitive) gmObject);
        } else {
            throw new GM_Exception (
                "JTSAdapter.export does not support type '" +
                gmObject.getClass ().getName () + "'!");
        }
        return geometry;
    }

    /**
     * Converts a JTS-<tt>Geometry</tt> object to a corresponding
     * <tt>GM_Object</tt>.
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
     * @param geometry the JTS-<tt>Geometry</tt> to be converted
     * @return the corresponding <tt>GM_Object</tt>
     * @throws GM_Exception if type unsupported or conversion failed
     */    
	public static GM_Object wrap (Geometry geometry)
        throws GM_Exception {

        GM_Object gmObject = null;
        if (geometry instanceof Point) {
            gmObject = wrap ((Point) geometry);
        } else if (geometry instanceof MultiPoint) {
            gmObject = wrap ((MultiPoint) geometry);
        } else if (geometry instanceof LineString) {
            gmObject = wrap ((LineString) geometry);
        } else if (geometry instanceof MultiLineString) {
            gmObject = wrap ((MultiLineString) geometry);            
        } else if (geometry instanceof Polygon) {
            gmObject = wrap ((Polygon) geometry);
        } else if (geometry instanceof MultiPolygon) {
            gmObject = wrap ((MultiPolygon) geometry);
        } else if (geometry instanceof GeometryCollection) {
            gmObject = wrap ((GeometryCollection) geometry);
        } else {
            throw new GM_Exception (
                "JTSAdapter.wrap does not support type '" +
                geometry.getClass ().getName () + "'!");
        }
        return gmObject;
    }

    /**
     * Converts a <tt>GM_Point</tt> to a <tt>Point</tt>.
     * <p>
     * @param gmPoint point to be converted
     * @return the corresponding <tt>Point</tt> object
     */
    private static Point export (GM_Point gmPoint) {
        Coordinate coord = new Coordinate (gmPoint.getX(), gmPoint.getY());
        return jtsFactory.createPoint (coord);
    }

    /**
     * Converts a <tt>GM_MultiPoint</tt> to a <tt>MultiPoint</tt>.
     * <p>
     * @param gmMultiPoint multipoint to be converted
     * @return the corresponding <tt>MultiPoint</tt> object
     */
    private static MultiPoint export (GM_MultiPoint gmMultiPoint) {
        GM_Point [] gmPoints = gmMultiPoint.getAllPoints ();
        Point [] points = new Point [gmPoints.length];
        for (int i = 0; i < points.length; i++) {
            points [i] = export (gmPoints [i]);
        }
        return jtsFactory.createMultiPoint (points);
    }

    /**
     * Converts a <tt>GM_Curve</tt> to a <tt>LineString</tt>.
     * <p>
     * @param curve <tt>GM_Curve</tt> to be converted
     * @return the corresponding <tt>LineString</tt> object
     * @throws GM_Exception
     */
    private static LineString export (GM_Curve curve)
        throws GM_Exception {

        GM_LineString lineString = curve.getAsLineString ();
        Coordinate [] coords = new Coordinate [lineString.getNumberOfPoints()];
        for (int i = 0; i < coords.length; i++) {
            GM_Position position = lineString.getPositionAt (i);           
            coords [i] = new Coordinate (position.getX(), position.getY ());
        }
        return jtsFactory.createLineString (coords);
    }    

    /**
     * Converts a <tt>GM_MultiCurve</tt> to a <tt>MultiLineString</tt>.
     * <p>
     * @param multi <tt>GM_MultiCurve</tt> to be converted
     * @return the corresponding <tt>MultiLineString</tt> object
     * @throws GM_Exception
     */
    private static MultiLineString export (GM_MultiCurve multi)
        throws GM_Exception {

        GM_Curve [] curves = multi.getAllCurves ();
        LineString [] lineStrings = new LineString [curves.length];
        for (int i = 0; i < curves.length; i++) {
            lineStrings [i] = export (curves [i]);
        }
        return jtsFactory.createMultiLineString (lineStrings);
    }
    
    /**
     * Converts an array of <tt>GM_Position</tt>s to a <tt>LinearRing</tt>.
     * <p>
     * @param positions an array of <tt>GM_Position</tt>s
     * @return the corresponding <tt>LinearRing</tt> object
     */
    private static LinearRing export (GM_Position [] positions) {
        Coordinate [] coords = new Coordinate [positions.length];
        for (int i = 0; i < positions.length; i++) {
            coords [i] = new Coordinate (positions [i].getX(), positions [i].getY());
        }
        return jtsFactory.createLinearRing (coords);
    }

    /**
     * Converts a <tt>GM_Surface</tt> to a <tt>Polygon</tt>.
     * <p>
     * Currently, the <tt>GM_Surface</tt> _must_ contain exactly one patch!
     * <p>
     * @param surface a <tt>GM_Surface</tt>
     * @return the corresponding <tt>Polygon</tt> object
     */    
    private static Polygon export (GM_Surface surface) {
        GM_SurfacePatch patch = null;
        try {
            patch = surface.getSurfacePatchAt (0);
        } catch (GM_Exception e) {
            System.out.println (e);
        }
        GM_Position [] exteriorRing = patch.getExteriorRing();
        GM_Position [] [] interiorRings = patch.getInteriorRings();

        LinearRing shell = export (exteriorRing);
        LinearRing [] holes = new LinearRing [0];
        if (interiorRings != null) holes = new LinearRing [interiorRings.length];
        for (int i = 0; i < holes.length; i++) {
            holes [i] = export (interiorRings [i]);
        }
        return jtsFactory.createPolygon (shell, holes);
    }

    /**
     * Converts a <tt>GM_MultiSurface</tt> to a <tt>MultiPolygon</tt>.
     * <p>
     * Currently, the contained <tt>GM_Surface</tt> _must_ have exactly one
     * patch!
     * <p>
     * @param msurface a <tt>GM_MultiSurface</tt>
     * @return the corresponding <tt>MultiPolygon</tt> object
     */    
    private static MultiPolygon export (GM_MultiSurface msurface) {

        GM_Surface [] surfaces = msurface.getAllSurfaces ();
        Polygon [] polygons = new Polygon [surfaces.length];
        
        for (int i = 0; i < surfaces.length; i++) {
            polygons [i] = export (surfaces [i]);
        }
        return jtsFactory.createMultiPolygon (polygons);
    }    

    /**
     * Converts a <tt>GM_MultiPrimitive</tt> to a <tt>GeometryCollection</tt>.
     * <p>
     * @param multi a <tt>GM_MultiPrimtive</tt>
     * @return the corresponding <tt>GeometryCollection</tt> object
     * @throws GM_Exception
     */    
    private static GeometryCollection export (GM_MultiPrimitive multi) 
        throws GM_Exception {

        GM_Object [] primitives = multi.getAllPrimitives ();
        Geometry [] geometries = new Geometry [primitives.length];
        
        for (int i = 0; i < primitives.length; i++) {
            geometries [i] = export(primitives [i]);
        }
        return jtsFactory.createGeometryCollection (geometries);
    }    

    /**
     * Converts a <tt>Point</tt> to a <tt>GM_Point</tt>s.
     * <p>
     * @param point a <tt>Point</tt> object
     * @return the corresponding <tt>GM_Point</tt>
     */    
    private static GM_Point wrap (Point point) {
        Coordinate coord = point.getCoordinate();        
        return new GM_Point_Impl (coord.x, coord.y, null);
    }    

    /**
     * Converts a <tt>MultiPoint</tt> to a <tt>GM_MultiPoint</tt>.
     * <p>
     * @param multi a <tt>MultiPoint</tt> object
     * @return the corresponding <tt>GM_MultiPoint</tt>
     */    
    private static GM_MultiPoint wrap (MultiPoint multi) {
        GM_Point [] gmPoints = new GM_Point [multi.getNumGeometries ()];
        for (int i = 0; i < gmPoints.length; i++) {
            gmPoints [i] = wrap ((Point) multi.getGeometryN (i));
        }
        return new GM_MultiPoint_Impl (gmPoints, null);
    }    

    /**
     * Converts a <tt>LineString</tt> to a <tt>GM_Curve</tt>.
     * <p>
     * @param line a <tt>LineString</tt> object
     * @return the corresponding <tt>GM_Curve</tt>
     * @throws GM_Exception
     */
    private static GM_Curve wrap (LineString line) throws GM_Exception {
        Coordinate [] coords = line.getCoordinates();
        GM_Position [] positions = new GM_Position [coords.length];
        for (int i = 0; i < coords.length; i++) {
            positions [i] = new GM_Position_Impl (coords [i].x, coords [i].y);
        }
        return GeometryFactory.createGM_Curve (positions, null);
    }

    /**
     * Converts a <tt>MultiLineString</tt> to a <tt>GM_MultiCurve</tt>.
     * <p>
     * @param multi a <tt>MultiLineString</tt> object
     * @return the corresponding <tt>GM_MultiCurve</tt>
     * @throws GM_Exception
     */
    private static GM_MultiCurve wrap (MultiLineString multi) throws GM_Exception {
        GM_Curve [] curves = new GM_Curve [multi.getNumGeometries ()];
        for (int i = 0; i < curves.length; i++) {
            curves [i] = wrap ((LineString) multi.getGeometryN (i));
        }                
        return GeometryFactory.createGM_MultiCurve (curves);
    }    
    
    /**
     *
     * Converts a <tt>Polygon</tt> to a <tt>GM_Surface</tt>.
     * <p>
     * @param polygon a <tt>Polygon</tt>
     * @return the corresponding <tt>GM_Surface</tt> object
     * @throws GM_Exception
     */    
    private static GM_Surface wrap (Polygon polygon) throws GM_Exception {

        GM_Position [] exteriorRing = createGMPositions (polygon.getExteriorRing ());
        GM_Position [] [] interiorRings = new GM_Position [polygon.getNumInteriorRing()] [];

        for (int i = 0; i < interiorRings.length; i++) {
            interiorRings [i] = createGMPositions (polygon.getInteriorRingN(i));
        }
        GM_SurfacePatch patch = new GM_Polygon_Impl (
            new GM_SurfaceInterpolation_Impl (), exteriorRing, interiorRings, null);

        return new GM_Surface_Impl (patch);
    }

    /**
     * Converts a <tt>MultiPolygon</tt> to a <tt>GM_MultiSurface</tt>.
     * <p>
     * @param multiPolygon a <tt>MultiPolygon</tt>
     * @return the corresponding <tt>GM_MultiSurface</tt> object
     * @throws GM_Exception
     */
    private static GM_MultiSurface wrap (MultiPolygon multiPolygon)
        throws GM_Exception {
        
        GM_Surface [] surfaces = new GM_Surface [multiPolygon.getNumGeometries()];
        for (int i = 0; i < surfaces.length; i++) {
            surfaces [i] = wrap ((Polygon) multiPolygon.getGeometryN (i));
        }
        return new GM_MultiSurface_Impl (surfaces);
    }

    /**
     * Converts a <tt>GeometryCollection</tt> to a <tt>GM_MultiPrimitve</tt>.
     * <p>
     * @param collection a <tt>GeometryCollection</tt>
     * @return the corresponding <tt>GM_MultiPrimitive</tt> object
     * @throws GM_Exception
     */
    private static GM_MultiPrimitive wrap (GeometryCollection collection)
        throws GM_Exception {

        GM_MultiPrimitive multi = new GM_MultiPrimitive_Impl (null);
        for (int i = 0; i < collection.getNumGeometries(); i++) {
            multi.setObjectAt (wrap (collection.getGeometryN (i)), i);
        }
        return multi;
    }

    /**
     * Converts a <tt>LineString</tt> to an array of <tt>GM_Position</tt>s.
     * <p>
     * @param line a <tt>LineString</tt> object
     * @return the corresponding array of <tt>GM_Position</tt>s
     */
    private static GM_Position [] createGMPositions (LineString line) {
        Coordinate [] coords = line.getCoordinates();
        GM_Position [] positions = new GM_Position [coords.length];
        for (int i = 0; i < coords.length; i++) {
            positions [i] = new GM_Position_Impl (coords [i].x, coords [i].y);
        }
        return positions;
    }
}
