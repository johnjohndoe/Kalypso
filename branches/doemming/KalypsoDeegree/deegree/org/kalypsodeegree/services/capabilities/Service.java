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
package org.deegree.services.capabilities;

import java.net.URL;

import org.deegree.ogcbasic.ContactInformation;


/**
 * The interface provides acces to the <Service> element of the  Capabilities XML
 * providing general metadata for the service as a whole. It shall include a
 * Name, Title, and Online Resource URL. Optionally, Abstract, Keyword List,
 * Contact Information, Fees, and Access Constraints may be provided. The meaning
 * of most of these elements is defined in [ISO 19115]. The Service Name shall
 * be "OGC:WMS" in the case of a Web Map Service.
 * <p>----------------------------------------------------------------------</p>
* @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @version 2002-03-01
 */
public interface Service {
    /**
     * returns the name of the service. Typically, the Name is a single word used
     * for machine-to-machine communication.
     */
    String getName();

    /**
     * Returns the title of the service. The Title is for the benefit of humans.
     * The Service Title is at the discretion of the provider, and should be
     * brief yet descriptive enough to identify this server in a menu with other
     * servers.
     * @see Service#getName()
     */
    String getTitle();

    /**
     * The Abstract element allows a descriptive narrative providing more
     * information about the enclosing object.
     */
    String getAbstract();

    /**
     *
     *
     * @return 
     */
    /**
     * A list of keywords or keyword phrases should be included to help catalog
     * searching. Currently, no controlled vocabulary has been defined.
     */
    String[] getKeywordList();

    /**
     * The OnlineResource element within the Service element can be used, for
     * example, to point to the web site of the service provider. There are other
     * OnlineResource elements used for the URL prefix of each supported operation.
     */
    URL getOnlineResource();

    /**
     * Returns informations who to contact for questions about the service. This
     * method returns <tt>null</tt> if no contact informations are available.
     */
    ContactInformation getContactInformation();

    /**
     * Returns fees assigned to the service. If no fees defined "none" will be
     * returned.
     */
    String getFees();

    /**
     * Returns access constraints assigned to the service. If no access
     * constraints are defined "none" will be returned.
     */
    String getAccessConstraints();
}