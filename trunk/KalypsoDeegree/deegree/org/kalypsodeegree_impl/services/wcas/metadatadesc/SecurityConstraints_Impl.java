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

import java.util.ArrayList;

import org.deegree.services.wcas.metadatadesc.*;

/**
 * SecurityConstraints_Impl.java
 *
 * Created on 16. September 2002, 10:35
 */
public class SecurityConstraints_Impl implements SecurityConstraints {

    ClassificationCode classificationcode = null;
    String classificationsystem = null;
    String handlingdescription = null;
    ArrayList uselimitation = null;
    String usernote = null;
    
    /** Creates a new instance of SecurityConstraints_Impl */
    public SecurityConstraints_Impl(ClassificationCode classificationcode,
                                    String classificationsystem,
                                    String handlingdescription,
                                    String[] uselimitation,
                                    String usernote) {
        
        this.uselimitation = new ArrayList();
        setClassificationCode(classificationcode);
        setClassificationSystem(classificationsystem);
        setHandlingDescription(handlingdescription);
        setUseLimitation(uselimitation);
        setUserNote(usernote);
    }

    /**
     *
     */
    public ClassificationCode getClassificationCode() {
        return classificationcode;
    }
    
    /**
     * @see getClassificationCode
     */
    public void setClassificationCode(ClassificationCode classificationcode) {
        this.classificationcode = classificationcode;
    }

    /** minOccurs="0"
     *
     */
    public String getClassificationSystem() {
        return classificationsystem;
    }
    
    /**
     * @see getClassificationSystem
     */
    public void setClassificationSystem(String classificationsystem) {
        this.classificationsystem = classificationsystem;
    }

    /** minOccurs="0"
     *
     */
    public String getHandlingDescription() {
        return handlingdescription;
    }
    
    /**
     * @see getHandlingDescription
     */
    public void setHandlingDescription(String handlingdescription) {
        this.handlingdescription = handlingdescription;
    }

    /** minOccurs="0" maxOccurs="unbounded"
     *
     */
    public String[] getUseLimitation() {
        return (String[])uselimitation.toArray( new String[uselimitation.size()] );
    }
    
    /**
     * @see getUseLimitation
     */
    public void addUseLimitation(String uselimitation) {
        this.uselimitation.add(uselimitation);
    }
        
    /**
     * @see getUseLimitation
     */
    public void setUseLimitation(String[] uselimitation) {
        this.uselimitation.clear();
        for (int i = 0; i < uselimitation.length; i++) {
            this.uselimitation.add( uselimitation[i] );
        }
    }

    /** minOccurs="0"
     *
     */
    public String getUserNote() {
        return usernote;
    }
    
    /**
     * @see getUserNote
     */
    public void setUserNote(String usernote) {
        this.usernote = usernote;
    }

	
	/**
 	 * to String method
 	 */
	public String toString() {
		String ret = null;
		ret = "classificationcode = " + classificationcode + "\n";
		ret += "classificationsystem = " + classificationsystem + "\n";
		ret += "handlingdescription = " + handlingdescription + "\n";
		ret += "uselimitation = " + uselimitation + "\n";
		ret += "usernote = " + usernote + "\n";
		return ret;
	}

}
