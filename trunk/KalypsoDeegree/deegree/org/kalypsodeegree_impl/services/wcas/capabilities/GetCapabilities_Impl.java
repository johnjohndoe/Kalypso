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

package org.deegree_impl.services.wcas.capabilities;

import org.deegree.services.capabilities.DCPType;
import org.deegree.services.wcas.capabilities.GetCapabilities;

/**
 * Describes the access/location to/of the capabilities informations
 * <p>
 * ------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:uzs6tr@uni-bonn.de">Axel Schaefer </a>
 * @version $Revision$ $Date$
 */

public class GetCapabilities_Impl extends RequestType_Impl implements GetCapabilities
{

  GetCapabilities_Impl( DCPType[] dcpTypes )
  {
    super( dcpTypes );
  }

}
/*
 * Changes to this class. What the people haven been up to:
 * 
 * $Log$
 * Revision 1.2  2004/08/30 00:36:42  doemming
 * *** empty log message ***
 * Revision 1.1.1.1 2004/05/11 16:43:26
 * doemming backup of local modified deegree sources
 * 
 * Revision 1.1.1.1 2002/09/25 16:01:30 poth no message
 * 
 * Revision 1.1 2002/08/19 15:58:38 ap no message
 * 
 *  
 */
