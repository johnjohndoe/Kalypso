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

import org.deegree.ogcbasic.ContactPersonPrimary;
import org.deegree.xml.Marshallable;
import org.deegree.xml.XMLTools;

/**
 * names the conatct based on ISO 19115
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:k.lupp@web.de">Katharina Lupp </a>
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider </a>
 * @version $Revision$
 */
public class ContactPersonPrimary_Impl implements ContactPersonPrimary, Marshallable
{

  private String contactOrganization = null;

  private String contactPerson = null;

  /**
   * constructor initializing the class with ContactPersonPrimary Strings
   */
  public ContactPersonPrimary_Impl( String contactPerson, String contactOrganization )
  {
    setContactPerson( contactPerson );
    setContactOrganization( contactOrganization );
  }

  /**
   * returns the name of the contact person
   */
  public String getContactPerson()
  {
    return contactPerson;
  }

  /**
   * sets the name of the contact person
   */
  public void setContactPerson( String contactPerson )
  {
    this.contactPerson = contactPerson;
  }

  /**
   * returns the name of the organization that can be contacted / the contact
   * person works at.
   */
  public String getContactOrganization()
  {
    return contactOrganization;
  }

  /**
   * sets the name of the organization that can be contacted / the contact
   * person works at.
   */
  public void setContactOrganization( String contactOrganization )
  {
    this.contactOrganization = contactOrganization;
  }

  /**
   * Returns an XML representation of this object.
   */
  public String exportAsXML()
  {
    StringBuffer sb = new StringBuffer();

    sb.append( "<ContactPersonPrimary>" ).append( "<ContactPerson>" ).append(
        XMLTools.validateCDATA( contactPerson ) ).append( "</ContactPerson>" ).append(
        "<ContactOrganization>" ).append( XMLTools.validateCDATA( contactOrganization ) ).append(
        "</ContactOrganization>" ).append( "</ContactPersonPrimary>" );

    return sb.toString();
  }
}