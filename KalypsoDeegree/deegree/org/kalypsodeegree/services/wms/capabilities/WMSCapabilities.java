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

import org.deegree.services.capabilities.OGCWebServiceCapabilities;

/**
 * The purpose of the GetCapabilities operation is described in the Basic
 * Service Elements section, above. In the particular case of a Web Map Service,
 * the response of a GetCapabilities request is general information about the
 * service itself and specific information about the available maps.
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version 2002-03-01
 */
public interface WMSCapabilities extends OGCWebServiceCapabilities
{
  /**
   * The <Capability>element of the Capabilities XML names the actual operations
   * that are supported by the service instance, the output formats offered for
   * those operations, and the URL prefix for each operation.
   */
  Capability getCapability();

  /**
   * returns deegree specific capabilities/coniguration parameters
   */
  DeegreeParam getDeegreeParam();

}