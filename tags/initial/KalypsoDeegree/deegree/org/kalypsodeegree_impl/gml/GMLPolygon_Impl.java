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

import org.deegree.gml.*;
import org.deegree.xml.*;

import org.deegree_impl.tools.*;

import org.w3c.dom.*;


/**
 *
 *
 * <p>----------------------------------------------------------</p>
 *
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @version 07.02.2001
 * <p>
 */
public class GMLPolygon_Impl extends GMLGeometry_Impl implements GMLPolygon {
    /**
     * Creates a new GMLPolygon_Impl object.
     *
     * @param element 
     */
    public GMLPolygon_Impl( Element element ) {
        super( element );
    }

    /**
     * factory method to create a GMLPolygon. the poly that will be
     * returned doesn't contain any ring
     */
    public static GMLPolygon createGMLPolygon( Document doc ) {
        Debug.debugMethodBegin( "GMLPolygon_Impl", "createGMLPolygon" );

        Element elem = doc.createElementNS( GMLGeometricMapping.GMLNS, "gml:Polygon" );
        GMLPolygon poly = new GMLPolygon_Impl( elem );

        Debug.debugMethodEnd();
        return poly;
    }

    /**
     *
     *
     * @return 
     */
    public Element getAsElement() {
        return element;
    }

    /**
     * returns the exterior ring of the polygon
     */
    public GMLLinearRing getExteriorRing() {
        Debug.debugMethodBegin( this, "getExteriorRing" );

        NodeList nl = element.getElementsByTagNameNS( GMLGeometricMapping.GMLNS, "outerBoundaryIs" );

        GMLLinearRing exring = new GMLLinearRing_Impl( (Element)nl.item( 0 ) );

        Debug.debugMethodEnd();
        return exring;
    }

    /**
     * @see org.deegree_impl.gml.GMLPolygon_Impl#getExteriorRing()<p>
     * If an exterior ring already exsists, it will be removed
     * before setting the new one.
     */
    public void setExteriorRing( GMLLinearRing exteriorRing ) {
        Debug.debugMethodBegin( this, "getExteriorRing" );

        NodeList nl = element.getElementsByTagNameNS( GMLGeometricMapping.GMLNS, "outerBoundaryIs" );

        // remove exterior ring if already exists
        if ( ( nl != null ) && ( nl.getLength() > 0 ) ) {
            element.removeChild( nl.item( 0 ) );
        }

        Element elem = element.getOwnerDocument()
                              .createElementNS( GMLGeometricMapping.GMLNS, "gml:outerBoundaryIs" );
        element.appendChild( elem );

        // insert the submitted ring
        XMLTools.insertNodeInto( ( (GMLLinearRing_Impl)exteriorRing ).getAsElement(), elem );

        Debug.debugMethodEnd();
    }

    /**
     * returns the interior rings of the polygon. if no
     * interior rings exists null should be returned
     */
    public GMLLinearRing[] getInteriorRings() {
        Debug.debugMethodBegin( "", "getInteriorRings" );

        NodeList nl = element.getElementsByTagNameNS( GMLGeometricMapping.GMLNS, "innerBoundaryIs" );

        GMLLinearRing[] lrs = null;

        if ( ( nl != null ) && ( nl.getLength() > 0 ) ) {
            // get memory for every inner ring of the polygon
            lrs = new GMLLinearRing[nl.getLength()];

            // loop over every inner boundary
            for ( int i = 0; i < nl.getLength(); i++ ) {
                // get the ring that builds the boundary
                NodeList nl_ = ( (Element)nl.item( i ) ).getElementsByTagNameNS( 
                                       GMLGeometricMapping.GMLNS, "LinearRing" );
                lrs[i] = new GMLLinearRing_Impl( (Element)nl_.item( 0 ) );
            }
        }

        Debug.debugMethodEnd();
        return lrs;
    }

    /**
     * adds a interior ring to the polygon. if the submitted
     * ring isn't not completly contained within the exterior
     * ring an exception should be thrown.
     */
    public void addInteriorRing( GMLLinearRing interiorRing ) throws GMLException {
        Debug.debugMethodBegin( this, "addInteriorRing" );

        //TODO
        // check if submitted interior ring is located completly
        // within the exterior ring of the polygon
        Element elem = element.getOwnerDocument()
                              .createElementNS( GMLGeometricMapping.GMLNS, "gml:innerBoundaryIs" );
        element.appendChild( elem );

        // insert the submitted ring
        XMLTools.insertNodeInto( ( (GMLLinearRing_Impl)interiorRing ).getAsElement(), elem );

        Debug.debugMethodEnd();
    }

  
}

/*
 * Changes to this class. What the people haven been up to:
 *
 * $Log$
 * Revision 1.1  2004/05/11 16:43:24  doemming
 * Initial revision
 *
 * Revision 1.4  2004/03/02 07:38:14  poth
 * no message
 *
 * Revision 1.3  2003/11/26 17:05:35  poth
 * no message
 *
 * Revision 1.2  2003/04/23 15:44:40  poth
 * no message
 *
 * Revision 1.1.1.1  2002/09/25 16:01:05  poth
 * no message
 *
 * Revision 1.6  2002/08/19 15:59:29  ap
 * no message
 *
 * Revision 1.5  2002/08/05 16:11:02  ap
 * no message
 *
 * Revision 1.4  2002/08/01 08:56:56  ap
 * no message
 */
