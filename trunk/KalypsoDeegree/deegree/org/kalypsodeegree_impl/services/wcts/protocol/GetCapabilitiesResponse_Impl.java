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
package org.deegree_impl.services.wcts.protocol;

import org.deegree.services.OGCWebServiceRequest;
import org.deegree.services.wcts.protocol.GetCapabilitiesResponse;
import org.deegree_impl.services.OGCWebServiceResponse_Impl;
import org.w3c.dom.Document;

/**
 * The response of an GetCapabilities-Request is a XML-Document which is
 * according in its structure to the Capabilities-documents of the WFS or the
 * WMS 1.1.1 The root-element of the Capabilities-document is the
 * &lt;CTS_Capabilities&gt;. It contains toe It contains two elements of which
 * the first one serves the general descritpion of the service and the second
 * one desribes the operation which the server can transact.
 * 
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version 2002-07-29
 */
public class GetCapabilitiesResponse_Impl extends OGCWebServiceResponse_Impl implements
    GetCapabilitiesResponse
{

  private String capabilities = null;

  /**
   * constructor of the CTS_CapabilitiesResponse
   */
  GetCapabilitiesResponse_Impl( OGCWebServiceRequest request, Document exception,
      String capabilities )
  {
    super( request, exception );
    setCapabilities( capabilities );
  }

  /**
   * gets the CTS_Capabilities
   */
  public String getCapabilities()
  {
    return capabilities;
  }

  /**
   * sets the CTS_Capabilities
   */
  public void setCapabilities( String capabilities )
  {
    this.capabilities = capabilities;
  }
}