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

/**
 * In addition to using named layers, it is also useful to be able to define
 * custom user-defined layers for rendering.
 * <p>
 * </p>
 * Since a layer is defined as a collection of potentially mixed-type features,
 * the UserLayer element must provide the means to identify the features to be
 * used. All features to be rendered are assumed to be fetched from a Web
 * Feature Server (WFS) or a Web Coverage Service (WCS, in which case the term
 * features is used loosely).
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
public interface UserLayer extends Layer
{

  /**
   * the method returns a remote web service that enables the access to data
   * that are not stored on same server as the WMS.
   * 
   * @return the RemoteOWS
   */
  RemoteOWS getRemoteOWS();

  /**
   * Sets the RemoteOWS
   * 
   * @param remoteOWS
   *          the RemoteOWS
   */
  void setRemoteOWS( RemoteOWS remoteOWS );
}