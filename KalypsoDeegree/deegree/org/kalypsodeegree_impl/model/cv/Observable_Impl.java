/*----------------    FILE HEADER  ------------------------------------------
 
This file is part of deegree (Java Framework for Geospatial Solutions).
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
package org.deegree_impl.model.cv;

import java.net.URL;
import org.deegree.model.coverage.Observable;

/**
 * The Observable interface provides a structured description of the observations 
 * (quantities or properties) reported in this range component. This description 
 * consists of a name, a free-text description, a docURL (an index into a registry 
 * of observation types); and a referenceSystem that associates the reported 
 * values with real-world quantities or categories
 *
 * <p>-----------------------------------------------------------------------</p>
 *
 * @author Andreas Poth
 * @version $Revision$ $Date$
 * <p>
 */
class Observable_Impl implements Observable {
    
    private String description      = null;
    private URL docURL              = null;
    private String name             = null;
    private Object referenceSystem  = null;
    
    Observable_Impl(String name, String description, URL docURL, 
                    Object referenceSystem)
    {
        this.name = name;
        this.docURL = docURL;
        this.description = description;
        this.referenceSystem = referenceSystem;
    }

    /** returns a short description of the <tt>Observable</tt>
     *
     */
    public String getDescription() {
        return description;
    }    
    
    /** returns an index into a registry of observation types
     *
     */
    public URL getDocURL() {
        return docURL;
    }
    
    /** returns the name of the <tt>Observable</tt>
     *
     */
    public String getName() {
        return name;
    }
    
    /** returns and a referenceSystem that associates the reported values with
     * real-world quantities or categories
     *
     */
    public Object getReferenceSystem() {
        return referenceSystem;
    }
    
}
