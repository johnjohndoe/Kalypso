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
package org.deegree_impl.services.wcts.protocol;

import org.deegree.services.OGCWebServiceRequest;
import org.deegree.services.wcts.protocol.IsTransformableResponse;
import org.deegree.xml.XMLTools;
import org.deegree_impl.services.OGCWebServiceResponse_Impl;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;


/**
 * <p>The response to a IsTransformable Request returns only ' true ' 
 * or ' false '; whether a transformation is possible or not an error 
 * arises during the treatment request and only then an exception is returned.</p>
 * <p>----------------------------------------------------------------------</p>
* @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @version 2002-07-10
 */
public class IsTransformableResponse_Impl extends OGCWebServiceResponse_Impl
    implements IsTransformableResponse {
    private boolean transformable = false;

    /**
     * constructor initializing the response with the &lt;Transformable&gt;
     */
    IsTransformableResponse_Impl( OGCWebServiceRequest request, Document exception, 
                                  boolean transformable ) {
        super( request, exception );
        setTransformable( transformable );
    }

    /**
     * returns the transformable-attribute
     */
    public boolean getTransformable() {
        return transformable;
    }

    /**
     * @see getTransformable
     */
    public void setTransformable( boolean transformable ) {
        this.transformable = transformable;
    }

    /**
     *
     *
     * @return 
     */
    public Document exportAsXML() {
        Document doc = XMLTools.create();
        Element elem = doc.createElement( "TransformableResponse" );
        doc.appendChild( elem );

        Attr attr = doc.createAttribute( "transformable" );
        attr.setValue( "" + transformable );
        elem.appendChild( attr );

        Document exDoc = this.getException();

        if ( exDoc != null ) {
            Element exElem = (Element)exDoc.getElementsByTagName( "Exception" ).item( 0 );
            XMLTools.insertNodeInto( exElem, elem );
        }

        return doc;
    }
}