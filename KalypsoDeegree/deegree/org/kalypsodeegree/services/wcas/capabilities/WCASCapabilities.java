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

package org.deegree.services.wcas.capabilities;

import java.net.URL;

import org.deegree.services.capabilities.OGCWebServiceCapabilities;
import org.deegree.services.wcas.metadatadesc.ISO19119;

/**
 * The parent element of the Capabilities document includes as children a
 * Service element with general information about the server, a Capability
 * element with specific information about the kinds of functionality offered by
 * the server and a featureTypeList element defining the list of all feature
 * types available from this server.
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
public interface WCASCapabilities extends OGCWebServiceCapabilities
{

  /**
   * A Capability lists available request types, how exceptions may be reported,
   * and whether any vendor-specific capabilities are defined. It also lists all
   * the feature types available from this feature server.
   */
  Capability getCapability();

  /**
   * returns the service metadata of the catalog
   */
  ISO19119 getISO19119();

  /**
   * return the URL (location) of the schema definition for the submitted format
   */
  URL getSchemaLocation( String format );

}