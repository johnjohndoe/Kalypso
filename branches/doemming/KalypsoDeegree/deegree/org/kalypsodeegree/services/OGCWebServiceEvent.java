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
 * This is the defining interface for event objects that contains a request,
 * a response, a message or an exception the should be made available for a
 * service.<p></p>
 * the kind of contained imformation can be determined by calling the
 * <tt>getType</tt> method.
 * <p>--------------------------------------------------------------------</p>
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @version 2002-04-16
 */
public interface OGCWebServiceEvent {
    public static final int REQUEST = 0;
    public static final int RESPONSE = 1;

    /**
     * returns the id of the of the request which performance caused the
     * event.
     */
    public String getId();

    /**
     * returns the type of event. possible values are:
     * <ul>
     * <li>REQUSET
     * <li>RESPONSE
     * </ul>
     * An EXCEPTION will allways be a response to a request or a message.
     */
    int getType();

    /**
     * if the event is a REQUEST type the method returns the request transported
     * by the event. otherwise <tt>null</tt> will be returned.
     */
    OGCWebServiceRequest getRequest();

    /**
     * if the event is a RESPONSE type the method returns the response transported
     * by the event. otherwise <tt>null</tt> will be returned.
     */
    OGCWebServiceResponse getResponse();

    /**
     * returns the client where to write the result/response or an 
     * error message to
     */
    OGCWebServiceClient getDestination();
}