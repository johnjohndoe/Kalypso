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
package org.deegree.services;

import java.util.HashMap;

/**
 * This is the base interface for all request on OGC Web Services (OWS). Each
 * class that capsulates a request against an OWS has to implements this
 * interface.
 * <p>
 * --------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version 2002-02-28
 */
public interface OGCWebServiceRequest
{
  /**
   * The REQUEST parameter indicates which service operation is being invoked.
   * The value shall be the name of one of the operations offered by the OGC Web
   * Service Instance.
   */
  String getRequest();

  /**
   * The required SERVICE parameter indicates which of the available service
   * types at a particular service instance is being invoked.
   */
  String getService();

  /**
   * Finally, the requests allow for optional vendor-specific parameters (VSPs)
   * that will enhance the results of a request. Typically, these are used for
   * private testing of non-standard functionality prior to possible
   * standardization. A generic client is not required or expected to make use
   * of these VSPs.
   */
  HashMap getVendorSpecificParameters();

  /**
   * Finally, the requests allow for optional vendor-specific parameters (VSPs)
   * that will enhance the results of a request. Typically, these are used for
   * private testing of non-standard functionality prior to possible
   * standardization. A generic client is not required or expected to make use
   * of these VSPs.
   */
  String getVendorSpecificParameter( String name );

  /**
   * returns the ID of a request
   */
  String getId();

  /**
   * returns the requested service version
   */
  String getVersion();

  /**
   * returns the URI of a HTTP GET request. If the request doesn't support HTTP
   * GET a <tt>WebServiceException</tt> will be thrown
   */
  String getRequestParameter() throws WebServiceException;
}