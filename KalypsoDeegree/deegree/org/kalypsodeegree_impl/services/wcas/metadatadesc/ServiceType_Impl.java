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

/*
 * ServiceType_Impl.java
 *
 * Created on 16. September 2002, 10:36
 */

package org.deegree_impl.services.wcas.metadatadesc;

/**
 *
 * @author  as
 */
public class ServiceType_Impl implements org.deegree.services.wcas.metadatadesc.ServiceType {
    
    private String namename = null;
    private String namevalue = null;

    /** Creates a new instance of ServiceType_Impl */
    public ServiceType_Impl(String namename, String namevalue) {
        setNameName(namename);
        setNameValue(namevalue);
    }

    /** returns the NameName
     * @return String
     *
     */
    public String getNameName() {
        return namename;
    }
    
    /**
     * @see getNameName
     */
    public void setNameName(String namename) {
        this.namename= namename;
    }

    /** @return the String namevalue
     *
     */
    public String getNameValue() {
        return namevalue;
    }
    
    /**
     * @see getNameValue
     */
    public void setNameValue(String namevalue) {
        this.namevalue = namevalue;
    }

	/**
     * to String method
     */
	public String toString() {
		String ret = null;
		ret = "namename = " + namename + "\n";
		ret += "namevalue = " + namevalue + "\n";
		return ret;
	}

}
