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

/**
 * the interface describes the administration of requests that are in progress
 * by the WFS/WCS/WMS/CS (Dispatcher)
 * <p>
 * ------------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
public interface RequestController
{
  /**
   * Adds a part of the whole request to the controller. The parameter
   * identifies the listener that's responsible for performing this part of the
   * request.
   */
  public void addRequestPart( Object obj );

  /**
   * return the request handled by the class
   */
  public OGCWebServiceRequest getRequest();

  /**
   * returns true if all operations of the request are performed
   */
  public boolean requestFinished();

  /**
   * adds a response to the request controlled by this class or a part of it.
   */
  public void addResponse( OGCWebServiceResponse response );

  /**
   * creates a resonse object merging all independed responses that has been
   * resulted from perfoming the request
   */
  public OGCWebServiceResponse getResponse() throws Exception;
}