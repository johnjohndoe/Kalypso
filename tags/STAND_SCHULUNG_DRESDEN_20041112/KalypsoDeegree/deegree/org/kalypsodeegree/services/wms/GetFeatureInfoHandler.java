// $Header:
// /cvsroot/deegree/deegree/org/deegree/services/wms/GetFeatureInfoHandler.java,v
// 1.1 2004/05/26 08:49:39 poth Exp $
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
package org.deegree.services.wms;

import org.deegree.services.WebServiceException;
import org.deegree.services.wms.protocol.WMSGetFeatureInfoResponse;

/**
 * 
 * 
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @author last edited by: $Author$
 * 
 * @version 1.0. $Revision$, $Date$
 * 
 * @since 1.1
 */
public interface GetFeatureInfoHandler
{
  /**
   * performs a GetFeatureInfo request and retruns the result encapsulated
   * within a <tt>WMSFeatureInfoResponse</tt> object.
   * <p>
   * The method throws an WebServiceException that only shall be thrown if an
   * fatal error occurs that makes it imposible to return a result. If something
   * wents wrong performing the request (none fatal error) The exception shall
   * be encapsulated within the response object to be returned to the client as
   * requested (GetFeatureInfo-Request EXCEPTION-Parameter).
   * 
   * @return response to the GetFeatureInfo response
   */
  public abstract WMSGetFeatureInfoResponse performGetFeatureInfo() throws WebServiceException;
}