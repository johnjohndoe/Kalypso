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

import org.deegree.services.capabilities.OGCWebServiceCapabilities;

/**
 * This is the base interface for all OGC conform web services that are part of
 * the deegree frame work. I just defines one method for notifying a service
 * that it should perform its work.
 * <p>
 * The event object submitted to <tt>doService</tt> shall contain informations
 * about the calling class and the request the should be performed by the
 * service.
 * <p>
 * --------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version 2002-04-16
 */
public interface OGCWebService
{
  /**
   * the implementation of this method performs the handling of the passed
   * OGCWebServiceEvent in an new own Thread. The receiver of the response to
   * the request must implement the OGCWebServiceClient interface
   * 
   * @param request
   *          request (WMS, WCS, WFS, WCAS, WCTS, WTS, Gazetter) to perform
   * 
   * @throws WebServiceException
   */
  void doService( OGCWebServiceEvent request ) throws WebServiceException;

  /**
   * the implementation of this method performs the handling of the passed
   * OGCWebServiceEvent directly and returns the result to the calling class/
   * method
   * 
   * @param request
   *          request (WMS, WCS, WFS, WCAS, WCTS, WTS, Gazetter) to perform
   * 
   * @throws WebServiceException
   */
  OGCWebServiceResponse doService( OGCWebServiceRequest request ) throws WebServiceException;

  /**
   * 
   * 
   * @return
   */
  OGCWebServiceCapabilities getCapabilities();
}