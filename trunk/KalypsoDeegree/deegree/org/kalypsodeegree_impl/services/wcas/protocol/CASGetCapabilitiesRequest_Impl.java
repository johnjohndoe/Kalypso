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

import java.util.HashMap;

import org.deegree.services.wcas.protocol.CASGetCapabilitiesRequest;
import org.deegree_impl.services.OGCWebServiceRequest_Impl;

/**
 * The class implements the get capabilities CAS request.
 * 
 * <p>
 * --------------------------------------------------------
 * </p>
 * 
 * @author Andreas Poth <a href="mailto:k.lupp@web.de">Katharina Lupp </a>
 * @version $Revision$ $Date$
 */
class CASGetCapabilitiesRequest_Impl extends OGCWebServiceRequest_Impl implements
    CASGetCapabilitiesRequest
{
  /**
   * Creates a new CASGetCapabilitiesRequest_Impl object.
   * 
   * @param version
   * @param id
   * @param vendorSpecificParameter
   */
  CASGetCapabilitiesRequest_Impl( String version, String id, HashMap vendorSpecificParameter )
  {
    super( "GetRecord", "WCAS", version, id, vendorSpecificParameter );
  }

  /**
   * 
   * 
   * @return
   */
  public String toString()
  {
    return this.getClass().getName();
  }
}

/*
 * Changes to this class. What the people haven been up to:
 * 
 * $Log$
 * Revision 1.2  2004/08/30 00:36:48  doemming
 * *** empty log message ***
 * Revision 1.1.1.1 2004/05/11
 * 16:43:26 doemming backup of local modified deegree sources
 * 
 * Revision 1.4 2004/03/12 15:56:48 poth no message
 * 
 * Revision 1.3 2004/01/08 09:50:23 poth no message
 * 
 * Revision 1.2 2003/04/07 07:26:14 poth no message
 * 
 * Revision 1.1.1.1 2002/09/25 16:01:33 poth no message
 *  
 */
