/*
 * ---------------- FILE HEADER ------------------------------------------
 * 
 * This file is part of deegree. Copyright (C) 2001 by: EXSE, Department of
 * Geography, University of Bonn http://www.giub.uni-bonn.de/exse/ lat/lon
 * Fitzke/Fretter/Poth GbR http://www.lat-lon.de
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * Andreas Poth lat/lon Fitzke/Fretter/Poth GbR Meckenheimer Allee 176 53115
 * Bonn Germany E-Mail: poth@lat-lon.de
 * 
 * Jens Fitzke Department of Geography University of Bonn Meckenheimer Allee 166
 * 53115 Bonn Germany E-Mail: jens.fitzke@uni-bonn.de
 * 
 * 
 * ---------------------------------------------------------------------------
 */

package org.deegree_impl.services.wcas.metadatadesc;

import org.deegree.services.wcas.metadatadesc.Address;
import org.deegree.services.wcas.metadatadesc.ContactInfo;
import org.deegree.services.wcas.metadatadesc.OnLineResource;
import org.deegree.services.wcas.metadatadesc.Phone;

/**
 * ContactInfo_Impl.java
 * 
 * Created on 16. September 2002, 09:57
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:schaefer@lat-lon.de">Axel Schaefer </a>
 * @version $Revision$ $Date$ *
 */
public class ContactInfo_Impl implements ContactInfo
{

  private Address address = null;

  private String contactinstructions = null;

  private String hoursofservice = null;

  private OnLineResource onlineresource = null;

  private Phone phone = null;

  /** Creates a new instance of ContactInfo_Impl */
  public ContactInfo_Impl( Address address, String contactinstructions, String hoursofservice,
      OnLineResource onlineresource, Phone phone )
  {
    setAddress( address );
    setContactInstructions( contactinstructions );
    setHoursOfService( hoursofservice );
    setOnLineResource( onlineresource );
    setPhone( phone );
  }

  /**
   * minOccurs="0"
   *  
   */
  public Address getAddress()
  {
    return address;
  }

  /**
   * @see getAddress
   */
  public void setAddress( Address address )
  {
    this.address = address;
  }

  /**
   * minOccurs="0"
   *  
   */
  public String getContactInstructions()
  {
    return contactinstructions;
  }

  /**
   * @see getContactInstructions
   */
  public void setContactInstructions( String contactinstructions )
  {
    this.contactinstructions = contactinstructions;
  }

  /**
   * minOccurs="0"
   *  
   */
  public String getHoursOfService()
  {
    return hoursofservice;
  }

  /**
   * @see getHoursOfService
   */
  public void setHoursOfService( String hoursofservice )
  {
    this.hoursofservice = hoursofservice;
  }

  /**
   * minOccurs="0"
   *  
   */
  public OnLineResource getOnLineResource()
  {
    return onlineresource;
  }

  /**
   * @see getOnLineResource
   */
  public void setOnLineResource( OnLineResource onlineresource )
  {
    this.onlineresource = onlineresource;
  }

  /**
   * minOccurs="0"
   *  
   */
  public Phone getPhone()
  {
    return phone;
  }

  /**
   * @see getPhone
   */
  public void setPhone( Phone phone )
  {
    this.phone = phone;
  }

  /**
   * to String method
   */
  public String toString()
  {
    String ret = null;
    ret = "address = " + address + "\n";
    ret += "contactinstructions = " + contactinstructions + "\n";
    ret += "hoursofservice = " + hoursofservice + "\n";
    ret += "onlineresource = " + onlineresource + "\n";
    ret += "phone = " + phone + "\n";
    return ret;
  }

}