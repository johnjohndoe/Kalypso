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
package org.deegree_impl.clients.wcasclient.model;

/**
 * This class encapsulates the contact information that will be preented to
 * a user in the detailed metadata view of the catalog. It shouldn't be mistaken
 * as a complete mapping of the contact-element defined in ISO 19115 
 *
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 */
public class Contact {
    private String city = null;
    private String country = null;
    private String email = null;
    private String facsimile = null;
    private String name = null;
    private String phone = null;
    private String postalCode = null;
    private String street = null;
    private String role = null;
    private String organisation = null;
    private String onlineResource = null;

    /**
     * Creates a new Contact object.
     *
     * @param name name of the person
     * @param facsimile facsimile address of the person
     * @param phone phone number of the person
     * @param street street where the person can be reached
     * @param city city where the person can be reached
     * @param postalCode postalCode of the city
     * @param country 
     * @param email email address of the person
     */
    public Contact( String name, String facsimile, String phone, String street, String city, 
                    String postalCode, String country, String email, String role,
                    String organisation, String onlineResource ) {
        setName( name );
        setFacsimile( facsimile );
        setPhone( phone );
        setStreet( street );
        setCity( city );
        setPostalCode( postalCode );
        setCountry( country );
        setEmail( email );
        setRole( role );
        setOrganisation( organisation );
        setOnlineResource( onlineResource );
    }

    /**
     * returns the name of the contact person
     *
     * @return 
     */
    public String getName() {
        return name;
    }

    /**
     * sets the name of the contact person
     *
     * @param name 
     */
    public void setName( String name ) {
        this.name = name;
    }

    /**
     * returns the facsimile numbe of the contact person
     *
     * @return 
     */
    public String getFacsimile() {
        return facsimile;
    }

    /**
     * sets the facsimile number of the contact person
     *
     * @param facsimile 
     */
    public void setFacsimile( String facsimile ) {
        this.facsimile = facsimile;
    }

    /**
     * returns the phone number of the contact person
     *
     * @return 
     */
    public String getPhone() {
        return phone;
    }

    /**
     * sets the phone number of the contact person
     *
     * @param phone 
     */
    public void setPhone( String phone ) {
        this.phone = phone;
    }

    /**
     * returns the street where the contact person can be reached
     *
     * @return 
     */
    public String getStreet() {
        return street;
    }

    /**
     * sets the street where the contact person can be reached
     *
     * @param street 
     */
    public void setStreet( String street ) {
        this.street = street;
    }

    /**
     * returns the city where the contact person can be reached
     *
     * @return 
     */
    public String getCity() {
        return city;
    }

    /**
     * sets the city name where the contact person can be reached
     *
     * @param city 
     */
    public void setCity( String city ) {
        this.city = city;
    }

    /**
     * returns the postal code of the city
     *
     * @return 
     */
    public String getPostalCode() {
        return postalCode;
    }

    /**
     * sets the postal code of the city
     *
     * @param postalCode 
     */
    public void setPostalCode( String postalCode ) {
        this.postalCode = postalCode;
    }

    /**
     * 
     *
     * @return 
     */
    public String getCountry() {
        return country;
    }

    /**
     *
     *
     * @param country 
     */
    public void setCountry( String country ) {
        this.country = country;
    }

    /**
     * returns the email address of the contact person
     *
     * @return 
     */
    public String getEmail() {
        return email;
    }

    /**
     * sets the email address of the contact person
     *
     * @param email 
     */
    public void setEmail( String email ) {
        this.email = email;
    }
 
    /**
     * returns the name of the organisation the contact person belongs to
     *
     * @return name of the organisation the contact person belongs to
     */
    public String getOrganisation() {
        return organisation;
    }
    
    /**
     * set the name of the organisation the contact person belongs to
     *
     * @param organisation name of the organisation the contact person belongs to
     */
    public void setOrganisation(String organisation) {
        this.organisation = organisation;
    }
    
    /**
     * returns the name or description of the contacts role
     *
     * @return name or description of the contacts role
     */
    public String getRole() {
        return role;
    }
    
    /**
     * set the name or description of the contacts role
     *
     * @param role name or description of the contacts role
     */
    public void setRole(String role) {
        this.role = role;
    }
    
    /**
     * returns the online resource (web address) of the contact 
     *
     * @return online resource (web address) of the contact
     */
    public String getOnlineResource() {
        return onlineResource;
    }
    
    /**
     * set the online resource (web address) of the contact
     *
     * @param onlineResource
     */
    public void setOnlineResource(String onlineResource) {
        this.onlineResource = onlineResource;
    }
    
    public String toString() {
        return "Contact:\n" +
               "city = " + city + "\n" +
               "country = " + country + "\n" +
               "email = " + email + "\n" +
               "facsimile = " + facsimile + "\n" +
               "name = " + name + "\n" +
               "phone = " + phone + "\n" +
               "postalCode = " + postalCode + "\n" +
               "street = " + street + "\n" +
               "role = " + role + "\n" +
               "onlineResource = " + onlineResource + "\n" +
               "organisation = " + organisation + "\n";
    }
}