/*----------------    FILE HEADER  ------------------------------------------

 This file is part of Deegree.
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

 Markus Mueller
 Department of Geography
 University of Bonn
 Meckenheimer Allee 166
 53115 Bonn
 Germany
 E-Mail: mm@giub.uni-bonn.de

 ---------------------------------------------------------------------------*/

package org.deegree.services.wcts.protocol;

import org.deegree.services.OGCWebServiceRequest;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * <p>
 * This interface describes requests, which have to examine the possibility to
 * transform one coordinate system into another.
 * <p>
 * Beside the source and the target coordinate-system, the format of the
 * return-value will be fixed.
 * <p>
 * Furtheron the service offers the possibility to declarate user-defined
 * coordinate-systems over the indication of all parameters both for the source
 * and the target coordinate-system.
 * <p>
 * As a rule every particular coordinate-system will be indicated by an
 * identifier.
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version 2002-07-19
 */

public interface IsTransformableRequest extends OGCWebServiceRequest
{

  /**
   * gets the version
   */
  public String getVersion();

  /**
   * gets the SourceCRS
   */
  public CS_CoordinateSystem getSourceCRS();

  /**
   * gets the DestinationCRS
   */
  public CS_CoordinateSystem getDestinationCRS();

}