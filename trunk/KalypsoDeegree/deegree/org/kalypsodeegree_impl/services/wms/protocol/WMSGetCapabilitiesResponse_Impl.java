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
package org.deegree_impl.services.wms.protocol;

import org.deegree.services.OGCWebServiceRequest;
import org.deegree.services.wms.capabilities.WMSCapabilities;
import org.deegree.services.wms.protocol.WMSGetCapabilitiesResponse;
import org.deegree_impl.services.OGCWebServiceResponse_Impl;
import org.w3c.dom.Document;

/**
 * 
 * 
 * <p>
 * --------------------------------------------------------
 * </p>
 * 
 * @author Katharina Lupp <a href="mailto:k.lupp@web.de">Katharina Lupp </a>
 * @version $Revision$ $Date$
 */
public class WMSGetCapabilitiesResponse_Impl extends OGCWebServiceResponse_Impl implements
    WMSGetCapabilitiesResponse
{
  private WMSCapabilities capabilities = null;

  /**
   * constructor initializing the class with the <WMSFilterServiceResponse>
   */
  WMSGetCapabilitiesResponse_Impl( OGCWebServiceRequest request, Document exception,
      WMSCapabilities capabilities )
  {
    super( request, exception );
    setCapabilities( capabilities );
  }

  /**
   * returns the capabilities as result of an GetCapabilities request. If an
   * excption raised processing the request or the request has been invalid
   * <tt>null</tt> will be returned.
   */
  public WMSCapabilities getCapabilities()
  {
    return capabilities;
  }

  /**
   * sets the capabilities as result of an GetCapabilities request. If an
   * excption raised processing the request or the request has been invalid
   * <tt>null</tt> will be returned.
   */
  public void setCapabilities( WMSCapabilities capabilities )
  {
    this.capabilities = capabilities;
  }
}