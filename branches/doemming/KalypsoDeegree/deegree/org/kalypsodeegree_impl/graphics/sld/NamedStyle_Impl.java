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
package org.deegree_impl.graphics.sld;

import org.deegree.xml.Marshallable;
import org.deegree_impl.tools.Debug;
import org.deegree.graphics.sld.*;


/**
 * A NamedStyle uses the "name" attribute to identify a style known to the WMS
 * <p>----------------------------------------------------------------------</p>
 *
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @version $Revision$ $Date$
 */
class NamedStyle_Impl extends Style_Impl implements NamedStyle, Marshallable {
    /**
     * Creates a new NamedStyle_Impl object.
     *
     * @param name 
     */
    NamedStyle_Impl( String name ) {
        super( name );
    }
    
    /**
     * exports the content of the NamedStyle as XML formated String
     *
     * @return xml representation of the NamedStyle
     */
    public String exportAsXML() {
        Debug.debugMethodBegin();
        
        StringBuffer sb = new StringBuffer(100);
        sb.append( "<NamedStyle>" );
        sb.append( "<Name>" ).append( name ).append( "</Name>" );     
        sb.append( "</NamedStyle>" );
        
        Debug.debugMethodEnd();
        return sb.toString();
    }
}