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
 * The class represents a complex feature property. A complex property
 * may be a GMLFeature (that may has complex properties too) or a
 * GMLFeatureCollection. A GMLGeometry isn't taken as complex.
 *
 * <p>----------------------------------------------------------</p>
 *
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @version 07.02.2001
 * <p>
 */
public class GMLComplexProperty_Impl extends GMLProperty_Impl implements GMLComplexProperty {
    /**
     * Creates a new GMLComplexProperty_Impl object.
     *
     * @param element 
     */
    public GMLComplexProperty_Impl( Element element ) {
        super( element );
    }

    /**
     * factory method to create a GMLGeoProperty. the property that
     * will be return doesn't contain a value.
     */
    public static GMLComplexProperty createGMLComplexProperty( Document doc, String propName )
                                                       throws GMLException {
        Debug.debugMethodBegin( "", "createGMLComplexProperty(Document, String)" );

        Element elem = doc.createElement( propName );

        GMLComplexProperty prop = new GMLComplexProperty_Impl( elem );

        Debug.debugMethodEnd();
        return prop;
    }

    /**
     * factory method to create a GMLGeoProperty. the property that
     * will be return doesn't contain a value.
     */
    public static GMLComplexProperty createGMLComplexProperty( Document doc, String propertyName, 
                                                               GMLFeature complex )
                                                       throws GMLException {
        Debug.debugMethodBegin( "", "createGMLComplexProperty(Document, String, GMLFeature)" );

        GMLComplexProperty prop = createGMLComplexProperty( doc, propertyName );
        prop.setComplexPropetryValue( complex );

        Debug.debugMethodEnd();
        return prop;
    }

    /**
     * returns the value of the property
     */
    public GMLFeature getComplexPropertyValue() {
        Debug.debugMethodBegin( this, "getComplexPropertyValue" );

        NodeList nl = element.getChildNodes();

        GMLFeature feature = null;

        if ( ( nl != null ) && ( nl.getLength() > 0 ) ) {
            for ( int i = 0; i < nl.getLength(); i++ ) {
                if ( nl.item( i ) instanceof Element ) {
                    feature = new GMLFeatureCollection_Impl( (Element)nl.item( i ) );
                }
            }
        }

        Debug.debugMethodEnd();
        return feature;
    }

    /**
     * @see org.deegree_impl.gml.GMLComplexProperty_Impl#getPropertyValue()
     */
    public void setComplexPropetryValue( GMLFeature value ) throws GMLException {
        Debug.debugMethodBegin( this, "setComplexPropetryValue" );

        NodeList nl = element.getChildNodes();

        if ( nl != null ) {
            for ( int i = 0; i < nl.getLength(); i++ ) {
                element.removeChild( nl.item( i ) );
            }
        }

        Element elem = ( (GMLFeature_Impl)value ).getAsElement();

        XMLTools.insertNodeInto( elem, element );

        Debug.debugMethodEnd();
    }

    /**
     * will be overwritten by this class in a way that
     * an exception will be thrown if the method is called.
     * Because it doesn't make any sense to set the value
     * of a geo property as a String.
     */
    public void setPropertyValue( String value ) {
        throw new NoSuchMethodError( "It doesn't make any sense to set the value " + 
                                     "of a geo property as a String" );
    }

    /**
     *
     *
     * @return 
     */
    public String toString() {
        return DOMPrinter.nodeToString( element, "" );
    }
}

/*
 * Changes to this class. What the people haven been up to:
 *
 * $Log$
 * Revision 1.1  2004/05/11 16:43:24  doemming
 * Initial revision
 *
 * Revision 1.6  2004/03/01 07:45:47  poth
 * no message
 *
 * Revision 1.5  2003/04/23 15:44:39  poth
 * no message
 *
 * Revision 1.4  2002/11/25 09:32:41  poth
 * no message
 *
 * Revision 1.3  2002/11/13 16:57:09  poth
 * no message
 *
 * Revision 1.2  2002/10/21 08:19:02  poth
 * no message
 *
 * Revision 1.1.1.1  2002/09/25 16:01:01  poth
 * no message
 *
 * Revision 1.5  2002/08/19 15:59:29  ap
 * no message
 *
 * Revision 1.4  2002/08/05 16:11:02  ap
 * no message
 *
 * Revision 1.3  2002/08/01 08:56:56  ap
 * no message
 *
 */
