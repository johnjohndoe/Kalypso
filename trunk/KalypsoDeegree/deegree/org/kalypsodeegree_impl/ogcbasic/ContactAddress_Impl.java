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

import org.deegree.ogcbasic.ContactAddress;
import org.deegree.xml.Marshallable;
import org.deegree.xml.XMLTools;

/**
 * specifies the data structure of a address and the access to its components
 * based on ISO 19115
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:k.lupp@web.de">Katharina Lupp </a>
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider </a>
 * @version $Revision$
 */
public class ContactAddress_Impl implements ContactAddress, Marshallable
{

  private String address = null;

  private String addressType = null;

  private String city = null;

  private String country = null;

  private String postCode = null;

  private String stateOrProvince = null;

  /**
   * default constructor
   */
  public ContactAddress_Impl()
  {}

  /**
   * constructor initializing the class with ContactAddress Strings
   */
  public ContactAddress_Impl( String addressType, String address, String city,
      String stateOrProvince, String postCode, String country )
  {
    setAddressType( addressType );
    setAddress( address );
    setCity( city );
    setStateOrProvince( stateOrProvince );
    setPostCode( postCode );
    setCountry( country );
  }

  /**
   * returns the address type. e.g. 'postal'
   */
  public String getAddressType()
  {
    return addressType;
  }

  /**
   * sets the address type. e.g. 'postal'
   */
  public void setAddressType( String addressType )
  {
    this.addressType = addressType;
  }

  /**
   * returns the address. usally this is the street and number of a building. It
   * also can be a p.o. box
   */
  public String getAddress()
  {
    return address;
  }

  /**
   * sets the address. usally this is the street and number of a building. It
   * also can be a p.o. box
   */
  public void setAddress( String address )
  {
    this.address = address;
  }

  /**
   * returns the name of the city
   */
  public String getCity()
  {
    return city;
  }

  /**
   * sets the name of the city
   */
  public void setCity( String city )
  {
    this.city = city;
  }

  /**
   * returns the name of the state or province of the address.
   */
  public String getStateOrProvince()
  {
    return stateOrProvince;
  }

  /**
   * sets the name of the state or province of the address.
   */
  public void setStateOrProvince( String stateOrProvince )
  {
    this.stateOrProvince = stateOrProvince;
  }

  /**
   * returns the post code. This doesn't contain an abbreviation for the country
   */
  public String getPostCode()
  {
    return postCode;
  }

  /**
   * sets the post code. This doesn't contain an abbreviation for the country
   */
  public void setPostCode( String postCode )
  {
    this.postCode = postCode;
  }

  /**
   * returns the name of the country. this should be the complete name and not
   * an abbreviation.
   */
  public String getCountry()
  {
    return country;
  }

  /**
   * sets the name of the country. this should be the complete name and not an
   * abbreviation.
   */
  public void setCountry( String country )
  {
    this.country = country;
  }

  /**
   * Returns an XML representation of this object.
   */
  public String exportAsXML()
  {
    StringBuffer sb = new StringBuffer();

    sb.append( "<ContactAddress>" ).append( "<AddressType>" ).append(
        XMLTools.validateCDATA( addressType ) ).append( "</AddressType>" ).append( "<Address>" )
        .append( XMLTools.validateCDATA( address ) ).append( "</Address>" ).append( "<City>" )
        .append( XMLTools.validateCDATA( city ) ).append( "</City>" ).append( "<StateOrProvince>" )
        .append( XMLTools.validateCDATA( stateOrProvince ) ).append( "</StateOrProvince>" ).append(
            "<Postcode>" ).append( XMLTools.validateCDATA( postCode ) ).append( "</Postcode>" )
        .append( "<Country>" ).append( XMLTools.validateCDATA( country ) ).append( "</Country>" )
        .append( "</ContactAddress>" );

    return sb.toString();
  }
}