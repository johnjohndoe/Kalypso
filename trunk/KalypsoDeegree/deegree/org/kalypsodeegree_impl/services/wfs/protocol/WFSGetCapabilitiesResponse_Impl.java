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
package org.deegree_impl.services.wfs.protocol;

import org.deegree.services.OGCWebServiceRequest;
import org.deegree.services.wfs.capabilities.WFSCapabilities;
import org.deegree.services.wfs.protocol.WFSGetCapabilitiesResponse;
import org.w3c.dom.Document;

/**
 * 
 * <p>
 * --------------------------------------------------------
 * </p>
 * 
 * @author Andreas Poth <a href="mailto:k.lupp@web.de">Katharina Lupp </a>
 * @version $Revision$ $Date$
 */
class WFSGetCapabilitiesResponse_Impl extends WFSBasicResponse_Impl implements
    WFSGetCapabilitiesResponse
{
  private WFSCapabilities response = null;

  /**
   * constructor
   */
  WFSGetCapabilitiesResponse_Impl( OGCWebServiceRequest request, Document exception,
      WFSCapabilities response )
  {
    super( request, exception, null );
    setResponse( response );
  }

  /**
   * returns the capabilities
   */
  public WFSCapabilities getResponse()
  {
    return response;
  }

  /**
   * sets the capabilities
   */
  public void setResponse( WFSCapabilities response )
  {
    this.response = response;
  }

  /**
   * 
   * 
   * @return
   */
  public String toString()
  {
    String ret = this.getClass().getName() + ":\n";
    ret += ( "response: " + response + "\n" );
    return ret;
  }
}

/*
 * Changes to this class. What the people haven been up to:
 * 
 * $Log$
 * Revision 1.2  2004/08/30 00:36:56  doemming
 * *** empty log message ***
 * Revision 1.1.1.1 2004/05/11
 * 16:43:25 doemming backup of local modified deegree sources
 * 
 * Revision 1.4 2004/03/12 15:56:49 poth no message
 * 
 * Revision 1.3 2003/04/23 07:27:41 poth no message
 * 
 * Revision 1.2 2003/04/07 07:26:54 poth no message
 * 
 * Revision 1.1.1.1 2002/09/25 16:01:26 poth no message
 * 
 * Revision 1.5 2002/08/15 10:01:40 ap no message
 * 
 * Revision 1.4 2002/08/09 15:36:30 ap no message
 * 
 * Revision 1.3 2002/05/14 14:39:51 ap no message
 * 
 * Revision 1.2 2002/05/13 16:11:02 ap no message
 * 
 * Revision 1.1 2002/05/06 16:02:22 ap no message
 * 
 *  
 */
