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

import org.deegree.services.OGCWebServiceRequest;
import org.deegree.services.wcas.protocol.CASGetCapabilitiesResponse;
import org.deegree.xml.DOMPrinter;
import org.deegree_impl.services.OGCWebServiceResponse_Impl;
import org.w3c.dom.Document;

/**
 * 
 * <p>
 * --------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
class CASGetCapabilitiesResponse_Impl extends OGCWebServiceResponse_Impl implements
    CASGetCapabilitiesResponse
{
  private Document response = null;

  /**
   * constructor
   */
  CASGetCapabilitiesResponse_Impl( OGCWebServiceRequest request, Document exception,
      Document response )
  {
    super( request, exception );
    setResponse( response );
  }

  /**
   * returns the capabilities as dom document
   */
  public Document getResponse()
  {
    return response;
  }

  /**
   * sets the the capabilities dom document
   */
  public void setResponse( Document response )
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
    ret += ( "response: " + DOMPrinter.nodeToString( response, "UTF-8" ) + "\n" );
    return ret;
  }
}

/*
 * Changes to this class. What the people haven been up to:
 * 
 * $Log$
 * Revision 1.3  2004/10/07 14:09:04  doemming
 * *** empty log message ***
 *
 * Revision 1.1  2004/09/02 23:56:51  doemming
 * *** empty log message ***
 * Revision 1.3 2004/08/31
 * 12:53:32 doemming *** empty log message *** Revision 1.3 2004/02/09 08:00:03
 * poth no message
 * 
 * Revision 1.2 2003/04/07 07:26:14 poth no message
 * 
 * Revision 1.1.1.1 2002/09/25 16:01:32 poth no message
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
