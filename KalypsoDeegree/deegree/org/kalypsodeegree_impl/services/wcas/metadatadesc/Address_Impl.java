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
 * Address_Impl.java
 *
 * <p>----------------------------------------------------------------------</p>
 * @author <a href="mailto:schaefer@lat-lon.de">Axel Schaefer</a>
 * @version $Revision$ $Date$
 */
public class Address_Impl implements Address {
    
    private String administrativearea = null;
    private String city = null;
    private String country = null;
    private ArrayList deliverypoint = null;
    private ArrayList electronicmailaddress = null;
    private String postalcode = null;
    
    
        
    /** Creates a new instance of Address_Impl */
    public Address_Impl(String administrativearea,
                        String city,
                        String country,
                        String[] deliverypoint,
                        String[] electronicmailaddress,
                        String postalcode) {
        
        this.deliverypoint = new ArrayList();
        this.electronicmailaddress = new ArrayList();
        
        setAdministrativeArea(administrativearea);
        setCity(city);
        setCountry(country);
        setDeliveryPoint(deliverypoint);
        setElectronicMailAddress(electronicmailaddress);
        setPostalCode(postalcode);
    }
    
    
    /** minOccurs="0"
     *
     */
    public String getAdministrativeArea() {
        return administrativearea;
    }
    
    /**
     * @see Address_Impl#getAdministrativeArea()
     */
    public void setAdministrativeArea(String administrativearea) {
        this.administrativearea = administrativearea;
    }
    
    
    /**
     * minOccurs="0"/>
     */
    public String getCity() {
        return city;
    }
    
    /**
     * @see Address_Impl#getCity()
     */
    public void setCity(String city) {
        this.city = city;
    }
    
    
    /** minOccurs="0"
     *
     */
    public String getCountry() {
        return country;
    }
    
    /**
     * @see Address_Impl#getCountry()
     */
    public void setCountry(String country) {
        this.country = country;
    }
    
    
    /** minOccurs="0" maxOccurs="unbounded"
     *
     */
    public String[] getDeliveryPoint() {
        return (String[])deliverypoint.toArray( new String[deliverypoint.size()] );
    }
    
    /**
     * @see Address_Impl#getDeliveryPoint()
     */
    public void addDeliveryPoint(String deliverypoint) {
        this.deliverypoint.add(deliverypoint);
    }
    
    /**
     * @see Address_Impl#getDeliveryPoint()
     */
    public void setDeliveryPoint(String[] deliverypoint) {
        this.deliverypoint.clear();
        for (int i = 0; i < deliverypoint.length; i++) {
            this.deliverypoint.add( deliverypoint[i] );
        }
    }
    
    
    /**
     * minOccurs="0" maxOccurs="unbounded"
     */
    public String[] getElectronicMailAddress() {
        return (String[])electronicmailaddress.toArray( new String[electronicmailaddress.size()] );
    }
    
    /**
     * @see Address_Impl#getElectronicMailAddress()
     */
    public void addElectronicMailAddress(String electronicmailaddress) {
        this.electronicmailaddress.add(electronicmailaddress);
    }
    
    /**
     * @see Address_Impl#getElectronicMailAddress()
     */
    public void setElectronicMailAddress(String[] electronicmailaddress) {
        this.electronicmailaddress.clear();
        for (int i = 0; i < electronicmailaddress.length; i++) {
            this.electronicmailaddress.add( electronicmailaddress[i] );
        }
    }
    
    
    /** minOccurs="0"
     *
     */
    public String getPostalCode() {
        return postalcode;
    }
    
    /**
     * @see Address_Impl#getPostalCode()
     */
    public void setPostalCode(String postalcode) {
        this.postalcode = postalcode;
    }
    
    
    /**
     * tpString method
     */
    public String toString() {
        String ret = "administrativearea = " + administrativearea + "\n" +
        "city = " + city + "\n" +
        "country = " + country + "\n" +
        "deliverypoint = " + deliverypoint + "\n" +
        "electronicmailaddress = " + electronicmailaddress + "\n" +
        "postalcode =" + postalcode + "\n";
        return ret;
    }
    
}
