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
package org.deegree.services.wfs.protocol;

import org.deegree.services.OGCWebServiceRequest;


/**
*  The Interface definies the basic WFS request.
*
* <p>--------------------------------------------------------</p>
*
* @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
* @version $Revision$ $Date$
*/
public interface WFSBasicRequest extends OGCWebServiceRequest {
    /**
     * The <Native> element is intended to allow access to vendor 
     * specific capabilities of any particular web feature server or datastore.
     * The <Native> tag simply delimits the vendor specific command or operation.
     */
    public WFSNative getNative();
   
}

/*
 * Changes to this class. What the people haven been up to:
 *
 * $Log$
 * Revision 1.1  2004/05/11 16:43:22  doemming
 * Initial revision
 *
 * Revision 1.5  2004/02/09 07:57:02  poth
 * no message
 *
 * Revision 1.4  2003/06/10 07:52:04  poth
 * no message
 *
 * Revision 1.3  2003/04/23 07:23:12  poth
 * no message
 *
 * Revision 1.2  2003/04/10 07:31:05  poth
 * no message
 *
 * Revision 1.1.1.1  2002/09/25 16:01:55  poth
 * no message
 *
 * Revision 1.5  2002/08/15 10:02:41  ap
 * no message
 *
 * Revision 1.4  2002/04/26 09:02:51  ap
 * no message
 *
 * Revision 1.2  2002/04/25 16:17:19  ap
 * no message
 *
 * Revision 1.1  2002/04/04 16:17:15  ap
 * no message
 *
 */
