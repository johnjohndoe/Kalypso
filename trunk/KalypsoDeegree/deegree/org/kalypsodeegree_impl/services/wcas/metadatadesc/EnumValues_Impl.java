/*
----------------    FILE HEADER  ------------------------------------------
 
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

package org.deegree_impl.services.wcas.metadatadesc;

import org.deegree.services.wcas.metadatadesc.*;

/**
 * EnumValues_Impl.java
 *
 * Created on 16. September 2002, 10:01
 * <p>----------------------------------------------------------------------</p>
 * @author <a href="mailto:schaefer@lat-lon.de">Axel Schaefer</a>
 * @version $Revision$ $Date$
 */

public class EnumValues_Impl implements EnumValues {
    
    Value value = null;
    String valuedescription = null;
    ValueOnLineResource valueonlineresource = null;
    String valuetitle = null;

    /** Creates a new instance of EnumValues_Impl */
    public EnumValues_Impl(Value value,
                           String valuedescription,
                           ValueOnLineResource valueonlineresource,
                           String valuetitle) {
        setValue(value);
        setValueDescription(valuedescription);
        setValueOnLineResource(valueonlineresource);
        setValueTitle(valuetitle);
    }

    /**
     * @return
     */
    public Value getValue() {
        return value;
    }
    
    /**
     * @see getValue
     */
    public void setValue(Value value) {
        this.value = value;
    }

    
    /**
     * minOccurs="0"
     * @return
     */
    public String getValueDescription() {
        return valuedescription;
    }
    
    /**
     * @see getValueDescription
     */
    public void setValueDescription(String valuedescription) {
        this.valuedescription = valuedescription;
    }

    
    /** minOccurs="0"
     * @return
     *
     */
    public ValueOnLineResource getValueOnLineResource() {
        return valueonlineresource;
    }
    
    /**
     * @see getValueOnLineResource
     */
    public void setValueOnLineResource(ValueOnLineResource valueonlineresource) {
        this.valueonlineresource = valueonlineresource;
    }

    
    /** minOccurs="0"
     * @return
     *
     */
    public String getValueTitle() {
        return valuetitle;
    }
    
    /**
     * @see getValueTitle
     */
    public void setValueTitle(String valuetitle) {
        this.valuetitle = valuetitle;
    }
    
    /**
     * to String method
     */
    public String toString() {
        String ret = null;
        ret = "value = " + value + "\n";
        ret += "valuedescription = "  + valuedescription + "\n";
        ret += "valueonlineresource = " + valueonlineresource + "\n";
        ret += "valuetitle= " + valuetitle + "\n";
	return ret;
    }

}
