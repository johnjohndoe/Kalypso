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
class GMLGeometryCollection_Impl extends GMLGeometry_Impl implements GMLGeometryCollection {
    /**
     * Creates a new GMLGeometryCollection_Impl object.
     *
     * @param element 
     */
    public GMLGeometryCollection_Impl( org.w3c.dom.Element element ) {
        super( element );
    }

    /**
     * returns the geometries contained within the collection
     */
    public GMLGeometry[] getGeometries() {
        Debug.debugMethodBegin( "", "getGeometries" );

        ArrayList list = new ArrayList();
        NodeList nl = element.getChildNodes();

        if ( ( nl != null ) && ( nl.getLength() > 0 ) ) {
            for ( int i = 0; i < nl.getLength(); i++ ) {
                if ( nl.item( i ) instanceof Element ) {
                    Element elem = XMLTools.getFirstElement(nl.item( i ));
                    if ( elem == null ) {
                    } else if ( XMLTools.toLocalName( elem.getNodeName() ).equals( "Point" ) ) {
                        list.add( new GMLPoint_Impl( elem ) );
                    } else if ( XMLTools.toLocalName( elem.getNodeName() ).equals( "LineString" ) ) {
                        list.add( new GMLLineString_Impl( elem ) );
                    } else if ( XMLTools.toLocalName( elem.getNodeName() ).equals( "Polygon" ) ) {
                        list.add( new GMLPolygon_Impl( elem ) );
                    } else if ( XMLTools.toLocalName( nl.item( i ).getNodeName() )
                                        .equals( "MultiPoint" ) ) {
                        list.add( new GMLMultiPoint_Impl( elem ) );
                    } else if ( XMLTools.toLocalName( nl.item( i ).getNodeName() )
                                        .equals( "MultiLineString" ) ) {
                        list.add( new GMLMultiLineString_Impl( elem ) );
                    } else if ( XMLTools.toLocalName( nl.item( i ).getNodeName() )
                                        .equals( "MultiPolygon" ) ) {
                        list.add( new GMLMultiPolygon_Impl( elem ) );
                    }
                }
            }
        }

        Debug.debugMethodEnd();
        return (GMLGeometry[])list.toArray( new GMLGeometry[list.size()] );
    }

    /**
     * @see org.deegree_impl.gml.GMLGeometryCollection_Impl#getGeometries()
     */
    public void addGeometry( GMLGeometry geometry ) {
        Debug.debugMethodBegin( this, "addGeometry" );

        Element elem = null;

        if ( geometry instanceof GMLPoint ) {
            elem = ( (GMLPoint_Impl)geometry ).getAsElement();
        } else if ( geometry instanceof GMLLineString ) {
            elem = ( (GMLLineString_Impl)geometry ).getAsElement();
        } else if ( geometry instanceof GMLPolygon ) {
            elem = ( (GMLPolygon_Impl)geometry ).getAsElement();
        } else if ( geometry instanceof GMLMultiPoint ) {
            elem = ( (GMLMultiPoint_Impl)geometry ).getAsElement();
        } else if ( geometry instanceof GMLMultiLineString ) {
            elem = ( (GMLMultiLineString_Impl)geometry ).getAsElement();
        } else if ( geometry instanceof GMLMultiPolygon ) {
            elem = ( (GMLMultiPolygon_Impl)geometry ).getAsElement();
        }

        Element elem_ = element.getOwnerDocument()
                               .createElementNS( GMLGeometricMapping.GMLNS, "gml:geometryMember" );
        XMLTools.insertNodeInto( elem, elem_ );
        element.appendChild( elem_ );

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
 * Revision 1.8  2004/03/02 07:38:14  poth
 * no message
 *
 * Revision 1.7  2003/11/26 17:05:35  poth
 * no message
 *
 * Revision 1.6  2003/07/14 09:12:45  poth
 * no message
 *
 * Revision 1.5  2003/05/15 09:37:40  poth
 * no message
 *
 * Revision 1.4  2003/05/07 12:47:23  axel_schaefer
 * no message
 *
 * Revision 1.3  2003/05/05 15:48:59  poth
 * no message
 *
 * Revision 1.1.1.1  2002/09/25 16:01:01  poth
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
 *
 */
