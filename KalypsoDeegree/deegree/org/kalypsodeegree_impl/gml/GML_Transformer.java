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

import java.util.StringTokenizer;

import org.deegree.gml.GMLBox;
import org.deegree.gml.GMLCoordinates;
import org.deegree.gml.GMLDocument;
import org.deegree.gml.GMLGeometry;
import org.deegree.gml.GMLLineString;
import org.deegree.gml.GMLLinearRing;
import org.deegree.gml.GMLMultiLineString;
import org.deegree.gml.GMLMultiPoint;
import org.deegree.gml.GMLMultiPolygon;
import org.deegree.gml.GMLPoint;
import org.deegree.gml.GMLPolygon;
import org.deegree.model.geometry.GM_Curve;
import org.deegree.model.geometry.GM_LineString;
import org.deegree.model.geometry.GM_Point;
import org.deegree.model.geometry.GM_Position;
import org.deegree.model.geometry.GM_Surface;
import org.deegree_impl.model.ct.MathTransform;
import org.deegree_impl.model.geometry.GMLAdapter;
import org.deegree_impl.tools.StringExtend;
import org.w3c.dom.Document;


/**
 * This class transforms GML-documents. Possible GML-geometries are:
 * GMLPoint, GMLMultiPoint, GMLLineString, GMLMultiLineString,
 * GMLPolygon and GMLMultiPolygon
 *
 * <p>----------------------------------------------------------------------</p>
 * @version 1.1
 * @author <a href="mailto:schaefer@lat-lon.de">Axel Schaefer</a>
 * @date: May 2002
 */
public class GML_Transformer {
    TransformationFactory transformationfactory = null;

    /**
     * the default constructor
     */
    public GML_Transformer() {
    }

    /**
     * This selects the GMLGeometry and leads it to the
     * corresponending transform-method
     */
    public GMLGeometry transform( GMLGeometry gmlgeo, String target_srs ) throws Exception {
        if ( gmlgeo instanceof GMLPoint ) {
            gmlgeo = transformPoint( (GMLPoint)gmlgeo, target_srs );
        } else if ( gmlgeo instanceof GMLMultiPoint ) {
            gmlgeo = transformMultiPoint( (GMLMultiPoint)gmlgeo, target_srs );
        } else if ( gmlgeo instanceof GMLLineString ) {
            gmlgeo = transformLineString( (GMLLineString)gmlgeo, target_srs );
        } else if ( gmlgeo instanceof GMLMultiLineString ) {
            gmlgeo = transformMultiLineString( (GMLMultiLineString)gmlgeo, target_srs );
        } else if ( gmlgeo instanceof GMLPolygon ) {
            gmlgeo = transformPolygon( (GMLPolygon)gmlgeo, target_srs );
        } else if ( gmlgeo instanceof GMLMultiPolygon ) {
            gmlgeo = transformMultiPolygon( (GMLMultiPolygon)gmlgeo, target_srs );
        } else if ( gmlgeo instanceof GMLBox ) {
            //gmlgeo = transformBoundingBox((GMLBox)gmlgeo, target_srs);
        }

        return gmlgeo;
    }

    /**
     * Transforms the target_srs (like EPSG 1234) to a qualified srsName attribute
     * (like http://www.opengis.net/gml/srs/epsg.xml#1234)
     * ONLY "EPSG 1234" is featured, not "EPSG_1234" (underlined).
     * @param ts target_srs
     */
    public String completeSrs( String ts ) {
        ts = ts.replace( ':', ' ' );

        StringTokenizer a = new StringTokenizer( ts, " " );
        return "http://www.opengis.net/gml/srs/" + a.nextToken().toLowerCase() + ".xml#" + 
               a.nextToken();
    }

    /**
     * Transforms the coords of a GMLPoint-geometry
     */
    public GMLPoint transformPoint( GMLPoint gmlgeo, String target_srs ) throws Exception {
        // creates an empty GMLPoint
        Document doc = ( (GMLPoint_Impl)gmlgeo ).getAsElement().getOwnerDocument();
        GMLPoint point = GMLPoint_Impl.createGMLPoint( doc );

        // Gets the SRS from the Source-GMLPoint
        String src_srs = gmlgeo.getSrs();

        // Creates the TransformationFactory and the MathTransform
        transformationfactory = new TransformationFactory();

        MathTransform transform = transformationfactory.getTransform( src_srs, target_srs );

        // Creates a GM_Point of the GMLPoint        
        GM_Point pnt = (GM_Point)GMLAdapter.wrap( gmlgeo );

        double[] coordarray = pnt.getAsArray();

        transform.transform( coordarray, 0, coordarray, 0, coordarray.length - 1 );

        int codeValue = 0;
        String code = target_srs.substring( 5 );

        if ( code.startsWith( "23" ) ) {
            String s = code.substring( 3 );
            codeValue = Integer.parseInt( s ) * 1000000;
        }

        coordarray[0] += codeValue;

        String string = StringExtend.arrayToString( coordarray, ',' );

        GMLCoordinates coordinates = GMLCoordinates_Impl.createGMLCoordinates( doc, "" );

        coordinates.setCoordinates( string );
        point.setCoordinates( coordinates );

        String complete_srs = completeSrs( target_srs );
        point.setSrs( complete_srs );

        return point;
    }

    /**
     *
     *
     * @param gmlgeo 
     * @param target_srs 
     *
     * @return 
     *
     * @throws Exception 
     */
    public GMLMultiPoint transformMultiPoint( GMLMultiPoint gmlgeo, String target_srs )
                                      throws Exception {

        //Creates the GMLMultiPoint
        Document doc = ( (GMLMultiPoint_Impl)gmlgeo ).getAsElement().getOwnerDocument();
        GMLMultiPoint mpoint = GMLMultiPoint_Impl.createGMLMultiPoint( doc );

        //Writes the points of the GMLMultiPoint on another GMLPoint array.
        GMLPoint[] p = gmlgeo.getPoints();

        // Every point in this array is given over to the transform point method.
        GMLPoint px = null;

        for ( int i = 0; i < p.length; i++ ) {
            // sets the SRS for each Point.
            p[i].setSrs( completeSrs( gmlgeo.getSrs() ) );
            px = transformPoint( p[i], target_srs );
            mpoint.addPoint( px );
        }

        mpoint.setSrs( completeSrs( target_srs ) );
        return mpoint;
    }

    /**
     * Transforms the coords of a GMLLineString-geometry
     */
    public GMLLineString transformLineString( GMLLineString gmlgeo, String target_srs )
                                      throws Exception {
        // Gets the SRS from the Source-GMLLineString
        String src_srs = gmlgeo.getSrs();

        // Creates the TransformationFactory and the MathTransform
        transformationfactory = new TransformationFactory();

        MathTransform transform = transformationfactory.getTransform( src_srs, target_srs );

        // creates an empty GMLLineString
        Document doc = ( (GMLLineString_Impl)gmlgeo ).getAsElement().getOwnerDocument();
        GMLLineString linestring = GMLLineString_Impl.createGMLLineString( doc );

        // creates the space for the GMLCoordinates in the GMLLineString
        GMLCoordinates coordinates = GMLCoordinates_Impl.createGMLCoordinates( doc, "" );

        coordinates.setCoordinateSeperator( ',' );
        coordinates.setDecimalSeperator( '.' );
        coordinates.setTupleSeperator( ' ' );

        GM_Curve curve = (GM_Curve)GMLAdapter.wrap( gmlgeo );

        GM_LineString ls = curve.getAsLineString();

        int codeValue = 0;
        String code = target_srs.substring( 5 );

        if ( code.startsWith( "23" ) ) {
            String s = code.substring( 3 );
            codeValue = Integer.parseInt( s ) * 1000000;
        }

        String s = "";

        for ( int i = 0; i < ls.getNumberOfPoints(); i++ ) {
            // ls.getNumberOfPoints = 3 (3 Punkte im Linestring)
            double[] d = ls.getPositionAt( i ).getAsArray();

            // d.length = 2 (x, y)
            transform.transform( d, 0, d, 0, d.length - 1 );

            s += ( StringExtend.arrayToString( d, ',' ) + " " );

            d[0] += codeValue;
        }

        coordinates.setCoordinates( s.trim() );

        linestring.setCoordinates( coordinates );

        linestring.setSrs( completeSrs( target_srs ) );

        return linestring;
    }

    /**
     * Transforms the coords of a GMLMultiLineString-geometry
     */
    public GMLMultiLineString transformMultiLineString( GMLMultiLineString gmlgeo, 
                                                        String target_srs )
                                                throws Exception {

        // creates an empty GMLMultiLineString
        Document doc = ( (GMLMultiLineString_Impl)gmlgeo ).getAsElement().getOwnerDocument();
        GMLMultiLineString multilinestring = GMLMultiLineString_Impl.createGMLMultiLineString( doc );
        multilinestring.setSrs( completeSrs( target_srs ) );

        GMLLineString[] linestrings = gmlgeo.getLineStrings();
        GMLLineString l = null;

        for ( int i = 0; i < linestrings.length; i++ ) {
            // sets the SRS for each LineString.
            linestrings[i].setSrs( completeSrs( gmlgeo.getSrs() ) );
            l = transformLineString( linestrings[i], target_srs );
            multilinestring.addLineString( l );
        }

        return multilinestring;
    }

    /**
     * Transforms the coords of a GMLPolygon-geometry
     */
    public GMLPolygon transformPolygon( GMLPolygon gmlgeo, String target_srs )
                                throws Exception {
        // creates the TransformationFactory
        String src_srs = gmlgeo.getSrs();
        transformationfactory = new TransformationFactory();

        MathTransform transform = transformationfactory.getTransform( src_srs, target_srs );

        // creates an empty GMLPolygon
        GMLDocument gml_ = new GMLDocument_Impl();
        Document doc = gml_.getDocument();
        GMLPolygon_Impl polygon = (GMLPolygon_Impl)GMLPolygon_Impl.createGMLPolygon( doc );
        polygon.setSrs( completeSrs( target_srs ) );

        // creates the space for the GMLCoordinates in the GMLLineString
        gml_ = new GMLDocument_Impl();

        GMLCoordinates coordinates = GMLCoordinates_Impl.createGMLCoordinates( gml_.getDocument(), 
                                                                               "" );
        coordinates.setCoordinateSeperator( ',' );
        coordinates.setDecimalSeperator( '.' );
        coordinates.setTupleSeperator( ' ' );

        GM_Surface surface = (GM_Surface)GMLAdapter.wrap( gmlgeo );

        // EXTERIOR-RING
        GM_Position[] gencurve = surface.getSurfacePatchAt( 0 ).getExteriorRing();

        int codeValue = 0;
        String code = target_srs.substring( 5 );

        if ( code.startsWith( "23" ) ) {
            String s = code.substring( 3 );
            codeValue = Integer.parseInt( s ) * 1000000;
        }

        String s = "";

        for ( int i = 0; i < gencurve.length; i++ ) {
            double[] d = gencurve[i].getAsArray();
            transform.transform( d, 0, d, 0, d.length - 1 );
            s += ( StringExtend.arrayToString( d, ',' ) + " " );
            d[0] += codeValue;
        }

        coordinates.setCoordinates( s.trim() );

        GMLLinearRing linring = GMLLinearRing_Impl.createGMLLinearRing( doc );
        linring.setCoordinates( coordinates );
        polygon.setExteriorRing( linring );

        // INTERIOR-RINGS
        GM_Position[][] gencurvearray = surface.getSurfacePatchAt( 0 ).getInteriorRings();

        String sint = "";
        double[] dint = null;

        gml_ = new GMLDocument_Impl();

        GMLLinearRing linringint = GMLLinearRing_Impl.createGMLLinearRing( gml_.getDocument() );

        if ( gencurvearray != null ) {
            // gencurvearray.length: number of inner-polygons
            for ( int i = 0; i < gencurvearray.length; i++ ) {
                // number of coordinates of the inner-polygon[i]
                for ( int j = 0; j < gencurvearray[i].length; j++ ) {
                    dint = gencurvearray[i][j].getAsArray();
                    transform.transform( dint, 0, dint, 0, dint.length - 1 );
                    sint += ( StringExtend.arrayToString( dint, ',' ) + " " );
                    dint[0] += codeValue;
                }

                coordinates.setCoordinates( sint.trim() );

                linringint.setCoordinates( coordinates );
                polygon.addInteriorRing( linringint );

                sint = "";

            }
        }

        return polygon;
    }

    /**
     * Transforms the coords of a GMLMultiPolygon-geometry
     */
    public GMLMultiPolygon transformMultiPolygon( GMLMultiPolygon gmlgeo, String target_srs )
                                          throws Exception {

        String complete_srs = completeSrs( target_srs );
        String gmlgeo_srs = completeSrs( gmlgeo.getSrs() );

        // creates an empty GMLMultiPolygon
        GMLDocument gml_ = new GMLDocument_Impl();
        Document doc_ = gml_.getDocument();
        GMLMultiPolygon result = GMLMultiPolygon_Impl.createGMLMultiPolygon( doc_ );
        result.setSrs( complete_srs );

        Document doc = ( (GMLMultiPolygon_Impl)gmlgeo ).getAsElement().getOwnerDocument();
        GMLMultiPolygon mp = GMLMultiPolygon_Impl.createGMLMultiPolygon( doc );
        mp.setSrs( complete_srs );

        GMLPolygon[] pa = gmlgeo.getPolygons();
        GMLPolygon p = null;

        for ( int i = 0; i < pa.length; i++ ) {
            // sets the SRS for each GMLPolygon
            pa[i].setSrs( gmlgeo_srs );

            p = transformPolygon( pa[i], target_srs );
            ( (GMLPolygon_Impl)p ).getAsElement().removeAttribute( "srsName" );
            result.addPolygon( p );
        }

        return result;
    }

    /**
     * since a GMLBoundingBox is a GMLGeometry (independent from the
     * ISO-Standard for geometries), the possiblity to transform a
     * GMLBoundingBox must be provided.
     */

    //    public GMLBox transformBoundingBox(GMLBox gmlgeo, String target_srs)
    //                                        throws Exception {
    //        
    //        // Gets the SRS from the Source-GMLBox
    //        String src_srs = gmlgeo.getSrs();
    //        
    //        // prepares the Tranformation (TransformationFactory + MathTransform)
    //        transformationfactory = new TransformationFactory();
    //        MathTransform transform = transformationfactory.getTransform(src_srs, target_srs);
    //        
    //        // completes the srs from source and target srs
    //        String complete_srs  = completeSrs( target_srs );
    //        String gmlgeo_srs = completeSrs(src_srs);
    //        
    //        // creates a new Document and a new element for the GMLLineString.
    //        // This GMLLineString is later given to the transformPoint-method
    //        Document ls_doc = XMLTools.create();
    //        Element ls_elem = ls_doc.createElementNS("http://www.opengis.net/gml", "gml:LineString");
    //        ls_elem.setAttribute( "xmlns:gml", "http://www.opengis.net/gml" );
    //        ls_doc.appendChild( ls_elem );
    //        // creates an empty GMLPoint
    //        GMLLineString ls = new GMLLineString_Impl(ls_elem);
    //       
    //        // creates a new Document and a new element for the GMLBox.
    //        // This GMLBox is later used for the returned result
    //        Document box_doc = XMLTools.create();
    //        Element box_elem = box_doc.createElementNS("http://www.opengis.net/gml", "gml:Box");
    //        box_elem.setAttribute( "xmlns:gml", "http://www.opengis.net/gml" );
    //        box_doc.appendChild( box_elem );
    //        // creates an empty GMLBox
    //        GMLBox result = new GMLBox_Impl(box_elem);
    //        
    //        // SRS of the resulting BBox
    //        result.setSrs( complete_srs );
    //        
    //        // Points
    //        ls.setSrs( gmlgeo_srs );
    //
    //        GMLCoord coordmax = null;
    //        GMLCoord coordmin = null;
    //        GMLCoordinates coordinates = gmlgeo.getCoordinates();
    //
    //        if (coordinates != null) {            
    //            // transforms coordinates
    //            ls.setCoordinates(coordinates);
    //            ls = transformLineString(ls, target_srs);
    //        } else {
    //            // transforms coords
    //            coordmin = gmlgeo.getMin();
    //            coordmax = gmlgeo.getMax();
    //            
    //            GMLCoord[] coordarray = new GMLCoord[]{coordmin, coordmax};
    //
    //            ls.setCoords(coordarray);
    //            ls = transformLineString(ls, target_srs);
    //            coordinates = ls.getCoordinates();
    //            result.setCoordinates(coordinates);
    //        }
    //        
    //        // sets the coordinates of the GMLBox with the coordinates of the
    //        // transformed LineString
    //        coordinates = ls.getCoordinates();
    //        result.setCoordinates(coordinates);
    //        return result;
    //    }
}