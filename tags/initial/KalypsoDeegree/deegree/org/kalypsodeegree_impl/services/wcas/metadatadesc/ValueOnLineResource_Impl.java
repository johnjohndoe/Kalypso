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
 * ValueOnlineResource_Impl.java
 *
 * Created on 16. September 2002, 10:40
 */
public class ValueOnLineResource_Impl implements ValueOnLineResource {
    
    private String applicationprofile = null;
    private FunctionCode functioncode = null;
    private Linkage linkage = null;
    private String onlineresourcedescription = null;
    private String onlineresourcename = null;
    private String protocol = null;
 

    /** Creates a new instance of ValueOnlineResource_Impl */
    public ValueOnLineResource_Impl(String applicationprofile,
                                    FunctionCode functioncode,
                                    Linkage linkage,
                                    String onlineresourcedescription,
                                    String onlineresourcename,
                                    String protocol) {
        
        setApplicationProfile(applicationprofile);
        setFunctionCode(functioncode);
        setLinkage(linkage);
        setOnlineResourceDescription(onlineresourcedescription);
        setOnlineResourceName(onlineresourcename);
        setProtocol(protocol); 
    }

    /** minOccurs="0"
     * @return
     *
     */
    public String getApplicationProfile() {
        return applicationprofile;
    }
    
    /**
     * @see getApplicationProfile
     */
    public void setApplicationProfile(String applicationprofile) {
        this.applicationprofile = applicationprofile;
    }

    
    /**
     * minOccurs="0"
     * @return
     */
    public FunctionCode getFunctionCode() {
        return functioncode;
    }
    
    /**
     * @see getFunctionCode
     */
    public void setFunctionCode(FunctionCode functioncode) {
        this.functioncode = functioncode;
    }
    

    /**
     * @return
     */
    public Linkage getLinkage() {
        return linkage;
    }
    
    /**
     * @see getLinkage
     */
    public void setLinkage(Linkage linkage) {
        this.linkage = linkage;
    }
        

    /**
     * minOccurs="0"
     * @return
     */
    public String getOnlineResourceDescription() {
        return onlineresourcedescription;
    }
    
    /**
     * @see getOnlineResourceDescription
     */
    public void setOnlineResourceDescription(String onlineresourcedescription) {
        this.onlineresourcedescription = onlineresourcedescription;
    }
    

    /**
     * minOccurs="0"
     * @return
     */
    public String getOnlineResourceName() {
        return onlineresourcename;
    }
    
    /**
     * @see getOnlineResourceName
     */
    public void setOnlineResourceName(String onlineresourcename) {
        this.onlineresourcename = onlineresourcename;
    }

    
    /**
     * minOccurs="0"
     * @return
     */
    public String getProtocol() {
        return protocol;
    }
    
    /**
     * @see getProtocol
     */
    public void setProtocol(String protocol) {
        this.protocol = protocol;
    }

	/**
     * to String method
     */
	public String toString() {
		String ret = null;
		ret = "applicationprofile = " + applicationprofile + "\n";
		ret += "functioncode = " + functioncode + "\n";
		ret += "linkage = " + linkage + "\n";
		ret += "onlineresourcedescription = " + onlineresourcedescription + "\n";
		ret += "onlineresourcename = " + onlineresourcename + "\n";
		ret += "protocol = " + protocol + "\n";
		return ret;
	}

}
