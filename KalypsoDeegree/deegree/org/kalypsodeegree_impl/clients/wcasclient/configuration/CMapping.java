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
package org.deegree_impl.clients.wcasclient.configuration;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;


/**
 *
 * @author  administrator
 */
public class CMapping {
    // mapping between request (HTML-Form) field names (keys) and catalog elements (values)
    private HashMap mappingsC = null;
    // mapping between catalog elements (keys) and request (HTML-Form) field names (values)
    private HashMap mappingsF = null;

    /** Creates a new instance of CMapping */
    public CMapping( HashMap mappingsC ) {
        this.mappingsC = mappingsC;
        mappingsF = new HashMap();

        Iterator iterator = mappingsC.keySet().iterator();

        while ( iterator.hasNext() ) {
            String ff = (String)iterator.next();
            String[] cf = getCatalogElements( ff );

            for ( int i = 0; i < cf.length; i++ ) {
                ArrayList list = null;
                if ( mappingsF.get( cf[i] ) != null ) {
                    list = (ArrayList)mappingsF.get( cf[i] );                    
                } else {
                    list = new ArrayList();                    
                }
                list.add( ff );
                mappingsF.put( cf[i],  list );
            }            
        }
        iterator = mappingsF.keySet().iterator();
        while ( iterator.hasNext() ) {
            Object o = iterator.next();
            ArrayList list = (ArrayList)mappingsF.get( o );
            mappingsF.put( o, (String[])list.toArray( new String[3] ) );
        }
    }

    /**
     * returns the catalog (iso-) elements that shall be targeted by a html form 
     * element 
     */
    public String[] getCatalogElements( String formField ) {
        return (String[])mappingsC.get( formField );
    }

    /**
     * returns the html form elements that are associated to a catalog field 
     */
    public String[] getFormFields( String catalogField ) {
        return (String[])mappingsF.get( catalogField );
    }

    /**
     *
     *
     * @return 
     */
    public String toString() {
        String ret = mappingsC.toString() + "\n";
               ret += mappingsF.toString() + "\n";
        return ret;
    }
}