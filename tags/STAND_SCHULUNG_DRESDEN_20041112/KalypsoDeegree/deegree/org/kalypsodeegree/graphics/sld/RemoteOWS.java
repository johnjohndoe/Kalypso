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
package org.deegree.graphics.sld;

import java.net.URL;

/**
 * Since a layer is defined as a collection of potentially mixed-type features,
 * the UserLayer element must provide the means to identify the features to be
 * used. All features to be rendered are assumed to be fetched from a Web
 * Feature Server (WFS) or a Web Coverage Service (WCS, in which case the term
 * features is used loosely).
 * <p>
 * </p>
 * The remote server to be used is identified by RemoteOWS (OGC Web Service)
 * element.
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
public interface RemoteOWS
{

  final static public String WFS = "WFS";

  final static public String WCS = "WCS";

  /**
   * type of service that is represented by the remote ows. at the moment
   * <tt>WFS</tt> and <tt>WCS</tt> are possible values.
   * 
   * @return the type of the services
   */
  String getService();

  /**
   * Sets a Service
   * 
   * @param service
   *          the type of the services
   */
  void setService( String service );

  /**
   * address of the the ows as URL
   * 
   * @return the adress of the ows as URL
   */
  URL getOnlineResource();

  /**
   * sets the address of the the ows as URL
   * 
   * @param onlineResource
   *          the adress of the ows as URL
   */
  void setOnlineResource( URL onlineResource );
}