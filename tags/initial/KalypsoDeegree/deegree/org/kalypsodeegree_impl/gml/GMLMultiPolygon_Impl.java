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

import org.deegree.gml.GMLGeometry;
import org.deegree.gml.GMLMultiPolygon;
import org.deegree.gml.GMLPolygon;
import org.deegree_impl.tools.Debug;
import org.w3c.dom.Document;
import org.w3c.dom.Element;


/**
*
*
* <p>----------------------------------------------------------</p>
* 
* @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
* @version 07.02.2001
* <p>
*/
public class GMLMultiPolygon_Impl extends GMLGeometryCollection_Impl implements GMLMultiPolygon {
    /**
     * Creates a new GMLMultiPolygon_Impl object.
     *
     * @param element 
     */
    public GMLMultiPolygon_Impl( Element element ) {
        super( element );
    }

    /**
     * factory method to create a GMLMultiPolygon. the collection
     * that will be empty
     */
    public static GMLMultiPolygon createGMLMultiPolygon( Document doc ) {
        Debug.debugMethodBegin( "", "createGMLMultiPolygon" );

        Element elem = doc.createElementNS( GMLGeometricMapping.GMLNS, "gml:MultiPolygon" );
        GMLMultiPolygon ls = new GMLMultiPolygon_Impl( elem );

        Debug.debugMethodEnd();
        return ls;
    }

    /**
     * returns all polygons contained within the collection
     */
    public GMLPolygon[] getPolygons() {
        Debug.debugMethodBegin( this, "getPolygons" );

        GMLGeometry[] g = super.getGeometries();
        GMLPolygon[] p = new GMLPolygon[g.length];

        for ( int i = 0; i < g.length; i++ ) {
            p[i] = (GMLPolygon)g[i];
        }

        Debug.debugMethodEnd();
        return p;
    }

    /**
     * @see org.deegree_impl.gml.GMLMultiPolygon_Impl#getPolygons()
     */
    public void addPolygon( GMLPolygon polygon ) {
        super.addGeometry( polygon );
    }
   
}

/*
 * Changes to this class. What the people haven been up to:
 *
 * $Log$
 * Revision 1.1  2004/05/11 16:43:24  doemming
 * Initial revision
 *
 * Revision 1.5  2004/03/02 07:38:14  poth
 * no message
 *
 * Revision 1.4  2004/01/03 13:46:45  poth
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
