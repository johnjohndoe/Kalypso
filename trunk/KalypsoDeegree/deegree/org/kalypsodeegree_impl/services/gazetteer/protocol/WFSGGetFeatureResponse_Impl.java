// $Header:
// /var/lib/cvs/backupdeegree/deegree/org/deegree_impl/services/gazetteer/protocol/WFSGGetFeatureResponse_Impl.java,v
// 1.1.1.1 2004/05/11 16:43:27 doemming Exp $
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
package org.deegree_impl.services.gazetteer.protocol;

import org.deegree.services.OGCWebServiceRequest;
import org.deegree.services.gazetteer.protocol.WFSGGetFeatureResponse;
import org.deegree_impl.services.wfs.protocol.WFSGetFeatureResponse_Impl;
import org.w3c.dom.Document;

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
public class WFSGGetFeatureResponse_Impl extends WFSGetFeatureResponse_Impl implements
    WFSGGetFeatureResponse
{
  /**
   * constructor initializing the class with the <WFSLockFeatureResponse>
   */
  WFSGGetFeatureResponse_Impl( OGCWebServiceRequest request, String[] affectedFeatureTypes,
      Document exception, Object response )
  {
    super( request, affectedFeatureTypes, exception, response );
  }
}
/*******************************************************************************
 * Changes to this class. What the people have been up to: $Log:
 * WFSGGetFeatureResponse_Impl.java,v $ Revision 1.1.1.1 2004/05/11 16:43:27
 * doemming backup of local modified deegree sources
 * 
 * Revision 1.2 2004/03/26 11:19:31 poth no message
 * 
 * 
 *  
 ******************************************************************************/