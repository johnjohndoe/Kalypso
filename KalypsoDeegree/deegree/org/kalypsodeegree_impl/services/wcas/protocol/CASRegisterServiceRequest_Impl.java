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
package org.deegree_impl.services.wcas.protocol;

import java.net.URL;
import java.util.HashMap;

import org.deegree.services.wcas.protocol.CASRegisterServiceRequest;
import org.deegree_impl.services.OGCWebServiceRequest_Impl;

/**
 * A RegisterService request forces the catalog to call metadata of the service
 * identified by <tt>getServiceAddress</tt>. It's a 'pull' mechanism to add
 * metadata to the catalog. The service address must offer metadata about the
 * service in a form that is well known to the catalog:
 * <p>
 * --------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version 2002-04-16
 */
final public class CASRegisterServiceRequest_Impl extends OGCWebServiceRequest_Impl implements
    CASRegisterServiceRequest
{
  private String serviceOwnerContactInfo = null;

  private URL serviceAddress = null;

  private int harvestFrequency = -1;

  /**
   * Creates a new CASRegisterServiceRequest_Impl object.
   * 
   * @param version
   * @param id
   * @param vendorSpecificParameter
   * @param serviceAddress
   * @param serviceOwnerContactInfo
   * @param harvestFrequency
   */
  CASRegisterServiceRequest_Impl( String version, String id, HashMap vendorSpecificParameter,
      URL serviceAddress, String serviceOwnerContactInfo, int harvestFrequency )
  {
    super( "RegisterService", "WCAS", version, id, vendorSpecificParameter );
    setServiceAddress( serviceAddress );
    setServiceOwnerContactInfo( serviceOwnerContactInfo );
    setHarvestFrequency( harvestFrequency );
  }

  /**
   * returns the address where the catalog can access metadata about a service.
   */
  public URL getServiceAddress()
  {
    return serviceAddress;
  }

  /**
   * @see CASRegisterServiceRequest_Impl#getServiceAddress()
   */
  public void setServiceAddress( URL serviceAddress )
  {
    this.serviceAddress = serviceAddress;
  }

  /**
   * returns informations about the service owner. Contact point for
   * notification of events
   */
  public String getServiceOwnerContactInfo()
  {
    return serviceOwnerContactInfo;
  }

  /**
   * @see CASRegisterServiceRequest_Impl#getServiceOwnerContactInfo()
   */
  public void setServiceOwnerContactInfo( String serviceOwnerContactInfo )
  {
    this.serviceOwnerContactInfo = serviceOwnerContactInfo;
  }

  /**
   * Frequency for catalog to check for and harvest new version of the Service
   * metadata document
   */
  public int getHarvestFrequency()
  {
    return harvestFrequency;
  }

  /**
   * @see CASRegisterServiceRequest_Impl#getHarvestFrequency()
   */
  public void setHarvestFrequency( int harvestFrequency )
  {
    this.harvestFrequency = harvestFrequency;
  }

  /**
   * 
   * 
   * @return
   */
  public String toString()
  {
    String ret = this.getClass().getName() + ":\n";
    ret = "harvestFrequency = " + harvestFrequency + "\n";
    ret += ( "serviceAddress = " + serviceAddress + "\n" );
    ret += ( "serviceOwnerContactInfo = " + serviceOwnerContactInfo + "\n" );
    return ret;
  }
}