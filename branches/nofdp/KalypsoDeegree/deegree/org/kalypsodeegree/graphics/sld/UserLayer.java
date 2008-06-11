/** This file is part of kalypso/deegree.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * history:
 * 
 * Files in this package are originally taken from deegree and modified here
 * to fit in kalypso. As goals of kalypso differ from that one in deegree
 * interface-compatibility to deegree is wanted but not retained always. 
 * 
 * If you intend to use this software in other ways than in kalypso 
 * (e.g. OGC-web services), you should consider the latest version of deegree,
 * see http://www.deegree.org .
 *
 * all modifications are licensed as deegree, 
 * original copyright:
 *
 * Copyright (C) 2001 by:
 * EXSE, Department of Geography, University of Bonn
 * http://www.giub.uni-bonn.de/exse/
 * lat/lon GmbH
 * http://www.lat-lon.de
 */
package org.kalypsodeegree.graphics.sld;

/**
 * In addition to using named layers, it is also useful to be able to define custom user-defined layers for rendering.
 * <p>
 * </p>
 * Since a layer is defined as a collection of potentially mixed-type features, the UserLayer element must provide the
 * means to identify the features to be used. All features to be rendered are assumed to be fetched from a Web Feature
 * Server (WFS) or a Web Coverage Service (WCS, in which case the term features is used loosely).
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
   * the method returns a remote web service that enables the access to data that are not stored on same server as the
   * WMS.
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