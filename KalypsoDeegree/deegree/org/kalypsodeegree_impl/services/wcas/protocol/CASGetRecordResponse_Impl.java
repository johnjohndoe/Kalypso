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
import org.deegree.services.wcas.protocol.CASGetRecordResponse;
import org.deegree_impl.services.OGCWebServiceResponse_Impl;
import org.w3c.dom.Document;


/**
 *
 * <p>--------------------------------------------------------------------</p>
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @version 2002-04-16
 */
final public class CASGetRecordResponse_Impl extends OGCWebServiceResponse_Impl
    implements CASGetRecordResponse {
    private Object response = null;

    /**
     * Creates a new CASGetRecordResponse_Impl object.
     *
     * @param request 
     * @param exception 
     * @param response 
     */
    CASGetRecordResponse_Impl(OGCWebServiceRequest request, Document exception, 
                              Object response) {
        super(request, exception);
        this.response = response;
    }

    /**
     * returns the response from the GetRecordResponse
     */
    public Object getResponse() {
        return response;
    }

    /**
     *
     *
     * @return 
     */
    public String toString() {
        String ret = this.getClass().getName() + ":\n";
        ret = "response = " + response + "\n";
        return ret;
    }
}

/*
 * Changes to this class. What the people haven been up to:
 *
 * $Log$
 * Revision 1.1  2004/05/11 16:43:26  doemming
 * Initial revision
 *
 * Revision 1.5  2004/02/09 08:00:03  poth
 * no message
 *
 * Revision 1.4  2004/01/08 09:50:23  poth
 * no message
 *
 * Revision 1.3  2003/08/25 08:39:52  poth
 * no message
 *
 * Revision 1.2  2003/04/07 07:26:14  poth
 * no message
 *
 * Revision 1.1.1.1  2002/09/25 16:01:33  poth
 * no message
 *
 * Revision 1.4  2002/08/20 15:56:54  ap
 * no message
 *
 * Revision 1.3  2002/08/19 15:58:21  ap
 * no message
 *
 * Revision 1.2  2002/08/15 10:01:24  ap
 * no message
 *
 * Revision 1.1  2002/08/08 07:09:42  ap
 * no message
 *
 *
 */
