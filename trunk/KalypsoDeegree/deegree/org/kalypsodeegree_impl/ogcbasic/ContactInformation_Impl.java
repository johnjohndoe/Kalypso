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
package org.deegree_impl.ogcbasic;

import org.deegree.xml.XMLTools;
import org.deegree.xml.Marshallable;

import org.deegree.ogcbasic.ContactInformation;
import org.deegree.ogcbasic.ContactPersonPrimary;
import org.deegree.ogcbasic.ContactAddress;


/**
 * identification of, and means of communication with a person and/or
 * organization associated with the service/resource. based on ISO 19115
 * <p>----------------------------------------------------------------------</p>
 * @author <a href="mailto:k.lupp@web.de">Katharina Lupp</a>
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider</a>
 * @version $Revision$
 */
public class ContactInformation_Impl implements ContactInformation,
    Marshallable {

    private ContactAddress contactAddress = null;
    private ContactPersonPrimary contactPersonPrimary = null;
    private String contactElectronicMailAddress = null;
    private String contactFacsimileTelephone = null;
    private String contactPosition = null;
    private String contactVoiceTelephone = null;

    /**
    * constructor initializing the class with ContactInformation Strings
    */
    public ContactInformation_Impl( String contactPosition, String contactVoiceTelephone, 
                                    String contactFacsimileTelephone, 
                                    String contactElectronicMailAddress, 
                                    ContactPersonPrimary contactPersonPrimary, 
                                    ContactAddress contactAddress ) {
        setContactPosition( contactPosition );
        setContactVoiceTelephone( contactVoiceTelephone );
        setContactFacsimileTelephone( contactFacsimileTelephone );
        setContactElectronicMailAddress( contactElectronicMailAddress );
        setContactPersonPrimary( contactPersonPrimary );
        setContactAddress( contactAddress );
    }

    /**
     * returns a datastructure that contains the name of the contact person and
     * the organization he works for.
     */
    public ContactPersonPrimary getContactPersonPrimary() {
        return contactPersonPrimary;
    }

    /**
    * sets a datastructure that contains the name of the contact person and
    * the organization he works for.
    */
    public void setContactPersonPrimary( ContactPersonPrimary contactPersonPrimary ) {
        this.contactPersonPrimary = contactPersonPrimary;
    }

    /**
     * returns the positon of the contact person within its organization
     */
    public String getContactPosition() {
        return contactPosition;
    }

    /**
    * sets the positon of the contact person within its organization
    */
    public void setContactPosition( String contactPosition ) {
        this.contactPosition = contactPosition;
    }

    /**
     * returns the address where to reach to contact person
     */
    public ContactAddress getContactAddress() {
        return contactAddress;
    }

    /**
    * sets the address where to reach to contact person
    */
    public void setContactAddress( ContactAddress contactAddress ) {
        this.contactAddress = contactAddress;
    }

    /**
     * returns the voice Telephone number of the contact person
     */
    public String getContactVoiceTelephone() {
        return contactVoiceTelephone;
    }

    /**
    * sets the voice Telephone number of the contact person
    */
    public void setContactVoiceTelephone( String contactVoiceTelephone ) {
        this.contactVoiceTelephone = contactVoiceTelephone;
    }

    /**
     * returns the facsimile Telephone number of the contact person
     */
    public String getContactFacsimileTelephone() {
        return contactFacsimileTelephone;
    }

    /**
    * sets the facsimile Telephone number of the contact person
    */
    public void setContactFacsimileTelephone( String contactFacsimileTelephone ) {
        this.contactFacsimileTelephone = contactFacsimileTelephone;
    }

    /**
     * returns the email address of the contact person
     */
    public String getContactElectronicMailAddress() {
        return contactElectronicMailAddress;
    }

    /**
    * sets the email address of the contact person
    */
    public void setContactElectronicMailAddress( String contactElectronicMailAddress ) {
        this.contactElectronicMailAddress = contactElectronicMailAddress;
    }

    /**
     * Returns an XML representation of this object.
     */
    public String exportAsXML() {
        StringBuffer sb = new StringBuffer ();

        sb.append ("<ContactInformation>");
        
        if (contactPersonPrimary != null) {
            sb.append (((Marshallable) contactPersonPrimary).exportAsXML ());
        }

        if (contactPosition != null) {
            sb.append ("<ContactPosition>")
              .append (XMLTools.validateCDATA (contactPosition))
              .append ("</ContactPosition>");
        }

        if (contactAddress != null) {
            sb.append (((Marshallable) contactAddress).exportAsXML ());
        }        

        if (contactVoiceTelephone != null) {
            sb.append ("<ContactVoiceTelephone>")
              .append (XMLTools.validateCDATA (contactVoiceTelephone))
              .append ("</ContactVoiceTelephone>");
        }

        if (contactFacsimileTelephone != null) {
            sb.append ("<ContactFacsimileTelephone>")
              .append (XMLTools.validateCDATA (contactFacsimileTelephone))
              .append ("</ContactFacsimileTelephone>");
        }

        if (contactElectronicMailAddress != null) {
            sb.append ("<ContactElectronicMailAddress>")
              .append (XMLTools.validateCDATA (contactElectronicMailAddress))
              .append ("</ContactElectronicMailAddress>");
        }        
        
        sb.append ("</ContactInformation>");

        return sb.toString ();
    }    
}
