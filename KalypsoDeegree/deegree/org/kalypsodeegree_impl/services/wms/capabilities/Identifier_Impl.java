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
package org.deegree_impl.services.wms.capabilities;

import org.deegree.services.wms.capabilities.*;
import org.deegree.xml.Marshallable;
import org.deegree.xml.XMLTools;


/**
 * A Map Server may use zero or more Identifier elements to list ID numbers
 * or labels defined by a particular Authority. For example, the Global Change
 * Master Directory (gcmd.gsfc.nasa.gov) defines a DIF_ID label for every
 * dataset. The authority name and explanatory URL are defined in a separate
 * AuthorityURL element, which may be defined once and inherited by subsidiary
 * layers. Identifiers themselves are not inherited.
 * <p>----------------------------------------------------------------------</p>
 *
 * @author <a href="mailto:k.lupp@web.de">Katharina Lupp</a>
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider</a>
 * @version $Revision$
 */
class Identifier_Impl implements Identifier, Marshallable {
    private String authority = null;
    private String value = null;

    /**
    * default constructor
    */
    Identifier_Impl() {
    }

    /**
    * constructor initializing the class with the <Identifier>
    */
    Identifier_Impl( String value, String authority ) {
        setValue( value );
        setAuthority( authority );
    }

    /**
     * returns the value of the identifier. that may be a ID, a label
     * or something comparable
     */
    public String getValue() {
        return value;
    }

    /**
    * sets the value of the identifier. that may be a ID, a label
    * or something comparable
    */
    public void setValue( String value ) {
        this.value = value;
    }

    /**
     * returns the name of the authority that defines the identifier
     */
    public String getAuthority() {
        return authority;
    }

    /**
    * sets the name of the authority that defines the identifier
    */
    public void setAuthority( String authority ) {
        this.authority = authority;
    }

    /**
     *
     *
     * @return 
     */
    public String toString() {
        String ret = null;
        ret = "value = " + value + "\n";
        ret += ( "authority = " + authority + "\n" );
        return ret;
    }

    /**
     * Returns an XML representation of this object.
     */
    public String exportAsXML() {
        StringBuffer sb = new StringBuffer();

        sb.append( "<Identifier authority=\"" ).append( authority ).append( "\">" );

        if ( value != null ) {
            sb.append( XMLTools.validateCDATA( value ) );
        }

        sb.append( "</Identifier>" );

        return sb.toString();
    }
}
