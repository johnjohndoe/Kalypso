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
 * 
 * <p>----------------------------------------------------------------------</p>
 *
 * @author <a href="mailto:k.lupp@web.de">Katharina Lupp</a>
 * @version $Revision$ $Date$
 */
public class Extent_Impl implements Extent, Marshallable {
    private String name = null;
    private String value = null;

    /**
    * default constructor
    */
    Extent_Impl() {
    }

    /**
    * constructor initializing the class with the <Extent>
    */
    Extent_Impl( String value, String name ) {
        setName( name );
        setValue( value );
    }

    /**
     * returns the name of the extent
     * @return the name of the extent
     */
    public String getName() {
        return name;
    }

    /**
    * sets the name of the extent
    * @param name the name of the extent
    */
    public void setName( String name ) {
        this.name = name;
    }

    /**
     * returns the value of the extent
     * @return the value of the extent
     */
    public String getValue() {
        return value;
    }

    /**
    * sets the value of the extent
    * @param value the value of the extent
    */
    public void setValue( String value ) {
        this.value = value;
    }
    
    /**
     * exports the content of the FeatureTypeConstraint as XML formated String
     *
     * @return xml representation of the FeatureTypeConstraint
     */
    public String exportAsXML() {
        Debug.debugMethodBegin();
        
        StringBuffer sb = new StringBuffer(1000);
        sb.append( "<Extent>" );
        sb.append( "<Name>" ).append( name ).append( "</Name>" );
        sb.append( "<Value>" ).append( name ).append( "</Value>" );
        sb.append( "</Extent>" );
        
        Debug.debugMethodEnd();
        return sb.toString();
    }  
    
}