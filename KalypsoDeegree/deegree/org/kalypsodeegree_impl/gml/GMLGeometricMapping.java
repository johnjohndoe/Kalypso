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
package org.deegree_impl.gml;

import java.util.*;


/**
 * class used by the GML implementation for mapping formal and
 * descriptive property names.
 *
 * <p>----------------------------------------------------------</p>
 *
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @version 07.02.2001
 * <p>
 */
class GMLGeometricMapping {
    private static HashMap toFN = new HashMap();
    private static HashMap toGeoType = new HashMap();
    public static String GMLNS = "http://www.opengis.net/gml";
    //formal names
    public static final String BOUNDEDBY = "gml:boundedBy";
    public static final String POINTPROPERTY = "gml:pointProperty";
    public static final String LINESTRINGPROPERTY = "gml:lineStringProperty";
    public static final String POLYGONPROPERTY = "gml:polygonProperty";
    public static final String GEOMETRYPROPERTY = "gml:geometryProperty";
    public static final String MULTIPOINTPROPERTY = "gml:multiPointProperty";
    public static final String MULTILINESTRINGPROPERTY = "gml:multiLineStringProperty";
    public static final String MULTIPOLYGONPROPERTY = "gml:multiPolygonProperty";
    public static final String MULTIGEOMETRYPROPERTY = "gml:multiGeometryProperty";
    // descriptive names
    public static final String LOCATION = "gml:location";
    public static final String POSITION = "gml:position";
    public static final String CENTEROF = "gml:centerOf";
    public static final String CENTERLINEOF = "gml:centerLineOf";
    public static final String EDGEOF = "gml:edgeOf";
    public static final String EXTENTOF = "gml:extentOf";
    public static final String COVERAGE = "gml:coverage";
    public static final String MULTILOCATION = "gml:multiLocation";
    public static final String MULTIPOSITION = "gml:multiPosition";
    public static final String MULTICENTEROF = "gml:multiCenterOf";
    public static final String MULTICENTERLINEOF = "gml:multiCenterLineOf";
    public static final String MULTIEDGEOF = "gml:multiEdgeOf";
    public static final String MULTIEXTENTOF = "gml:multiExtentOf";
    public static final String MULTICOVERAGE = "gml:multiCoverage";
    //geometry types
    public static final String BOX = "gml:Box";
    public static final String POINT = "gml:Point";
    public static final String LINESTRING = "gml:LineString";
    public static final String POLYGON = "gml:Polygon";
    public static final String MULTIPOINT = "gml:MultiPoint";
    public static final String MULTILINESTRING = "gml:MultiLineString";
    public static final String MULTIPOLYGON = "gml:MultiPolygon";
    public static final String MULTIGEOMETRY = "gml:MultiGeometry";

    static {
        toFN.put( LOCATION, POINTPROPERTY );
        toFN.put( POSITION, POINTPROPERTY );
        toFN.put( CENTEROF, POINTPROPERTY );
        toFN.put( POINTPROPERTY, POINTPROPERTY );
        toFN.put( CENTERLINEOF, LINESTRINGPROPERTY );
        toFN.put( EDGEOF, LINESTRINGPROPERTY );
        toFN.put( LINESTRINGPROPERTY, LINESTRINGPROPERTY );
        toFN.put( EXTENTOF, POLYGONPROPERTY );
        toFN.put( COVERAGE, POLYGONPROPERTY );
        toFN.put( POLYGONPROPERTY, POLYGONPROPERTY );
        toFN.put( MULTILOCATION, MULTIPOINTPROPERTY );
        toFN.put( MULTIPOSITION, MULTIPOINTPROPERTY );
        toFN.put( MULTICENTEROF, MULTIPOINTPROPERTY );
        toFN.put( MULTIPOINTPROPERTY, MULTIPOINTPROPERTY );
        toFN.put( MULTICENTERLINEOF, MULTILINESTRINGPROPERTY );
        toFN.put( MULTIEDGEOF, MULTILINESTRINGPROPERTY );
        toFN.put( MULTILINESTRINGPROPERTY, MULTILINESTRINGPROPERTY );
        toFN.put( MULTIEXTENTOF, MULTIPOLYGONPROPERTY );
        toFN.put( MULTICOVERAGE, MULTIPOLYGONPROPERTY );
        toFN.put( MULTIPOLYGONPROPERTY, MULTIPOLYGONPROPERTY );

        toGeoType.put( BOUNDEDBY, BOX );
        toGeoType.put( POINTPROPERTY, POINT );
        toGeoType.put( LINESTRINGPROPERTY, LINESTRING );
        toGeoType.put( POLYGONPROPERTY, POLYGON );
        toGeoType.put( MULTIPOINTPROPERTY, MULTIPOINT );
        toGeoType.put( MULTILINESTRINGPROPERTY, MULTILINESTRING );
        toGeoType.put( MULTIPOLYGONPROPERTY, MULTIPOLYGON );
        toGeoType.put( MULTIGEOMETRYPROPERTY, MULTIGEOMETRY );
    }

    /**
     * returns the formal name of a descriptive property name
     */
    public static String getFormalName( String descriptiveName ) {
        return (String)toFN.get( descriptiveName );
    }

    /**
     * adds a association between a descriptive name and the
     * formal name
     */
    public static void addDN2FN( String descriptiveName, String formalName ) {
        toFN.put( descriptiveName, formalName );
    }

    /**
     * returns the name of the geometry type a formal property name
     * is associated with.
     */
    public static String getGeometryType( String formalName ) {
        return (String)toGeoType.get( formalName );
    }

    /**
     * adds a association between a formal name and a geometry type
     */
    public static void addFN2GT( String formalName, String geoType ) {
        toGeoType.put( formalName, geoType );
    }
}

/*
 * Changes to this class. What the people haven been up to:
 *
 * $Log$
 * Revision 1.1  2004/05/11 16:43:24  doemming
 * Initial revision
 *
 * Revision 1.2  2003/04/23 15:44:39  poth
 * no message
 *
 * Revision 1.1.1.1  2002/09/25 16:01:04  poth
 * no message
 *
 * Revision 1.4  2002/08/19 15:59:29  ap
 * no message
 *
 * Revision 1.3  2002/08/05 16:11:02  ap
 * no message
 *
 * Revision 1.2  2002/07/31 06:26:06  ap
 * no message
 *
 */
