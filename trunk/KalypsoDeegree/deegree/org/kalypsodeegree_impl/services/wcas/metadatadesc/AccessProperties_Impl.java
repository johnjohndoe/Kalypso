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
 * AccessProperties_Impl.java
 *
 * <p>----------------------------------------------------------------------</p>
 * @author <a href="mailto:schaefer@lat-lon.de">Axel Schaefer</a>
 * @version $Revision$ $Date$
 */

public class AccessProperties_Impl implements AccessProperties {
    
    private String fees = null;
    private String orderinginstructions = null;
    private String plannedavailabledatetime = null;
    private String turnaround = null;

    /** Creates a new instance of AccessProperties_Impl */
    public AccessProperties_Impl(String fees,
                                 String orderinginstructions,
                                 String plannedavailabledatetime,
                                 String turnaround) {
        setFees(fees);
        setOrderingInstructions(orderinginstructions);
        setPlannedAvailableDateTime(plannedavailabledatetime);
        setTurnaround(turnaround);
    }
    

    /**
     * minOccurs="0"
     * @return String
     */
    public String getFees() {
        return fees;
    }
    
    /**
     * @see AccessProperties_Impl#getFees()
     */
    public void setFees(String fees) {
        this.fees = fees;
    }

    /**
     * minOccurs="0"
     * @return String
     */
    public String getOrderingInstructions() {
        return orderinginstructions;
    }
    
    /**
     * @see AccessProperties_Impl#getOrderingInstructions()
     */
    public void setOrderingInstructions(String orderinginstructions) {
        this.orderinginstructions = orderinginstructions;
    }
    
    /**
     * minOccurs="0"
     * @return String
     *
     */
    public String getPlannedAvailableDateTime() {
        return plannedavailabledatetime;
    }
    
    /**
     * @see AccessProperties_Impl#getPlannedAvailableDateTime()
     */
    public void setPlannedAvailableDateTime(String plannedavailabledatetime) {
        this.plannedavailabledatetime = plannedavailabledatetime;
    }
    
    
    /**
     * minOccurs="0"
     * @return String
     *
     */
    public String getTurnaround() {
        return turnaround;
    }
    
    /**
     * @see AccessProperties_Impl#getTurnaround()
     */
    public void setTurnaround(String turnaround) {
        this.turnaround = turnaround;
    }
    
    /**
     * to String method
     */
    public String toString() {
        String ret = null;
        ret = "fees = " + fees + "\n";
        ret += "orderinginstructions = " + orderinginstructions + "\n";
        ret += "plannedavailabledatetime = " + plannedavailabledatetime + "\n";
        ret += "turnaround = " + turnaround + "\n";
	return ret;
    }
    
    
}
