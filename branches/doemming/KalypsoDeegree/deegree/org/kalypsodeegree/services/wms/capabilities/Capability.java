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
package org.deegree.services.wms.capabilities;

import org.deegree.services.capabilities.*;

import org.w3c.dom.Document;


/**
 * The <Capability> element of the Capabilities XML names the actual operations
 * that are supported by the service instance, the output formats offered for
 * those operations, and the URL prefix for each operation. The XML DTD includes
 * placeholders for Distributed Computing Platforms other than HTTP, and request
 * methods other that HTTP GET, but currently only HTTP GET is defined for a
 * basic WMS.<p></p>
 * Ignorable vendor-specific elements may be included. An SLD WMS would also
 * include a <UserDefinedSymbolization> element and URLs for HTTP POST requests.
 *
 * <p>----------------------------------------------------------------------</p>
 *
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @version 2002-03-01
 */
public interface Capability {
    /**
     * Available WMS Operations are listed in a Request element.
     */
    Request getRequest();

    /**
     * returns the format where exceptions will be returned
     */
    CException getException();

    /**
     * retruns vendor specific capabilities that are not common
     * to wms as dom document.
     */
    Document getVendorSpecificCapabilities();

    /**
     * 
     */
    UserDefinedSymbolization getUserDefinedSymbolization();

    /**
     * returns the top level layer that may encloses several more
     * layers and layer hierachies available by a map server. If no
     * layer is available <tt>null</tt> will be returned.
     */
    Layer getLayer();

    /**
     * Returns the Layer identified by the submitted name. If no Layer matches
     * the name <tt>null</tt> will be returned.
     *
     * @param name name of the requested layer
     *
     * @return a layer object or <tt>null</tt>
     */
    Layer getLayer( String name );
}