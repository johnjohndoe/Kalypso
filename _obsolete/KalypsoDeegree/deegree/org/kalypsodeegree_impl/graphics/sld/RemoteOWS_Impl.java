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
package org.kalypsodeegree_impl.graphics.sld;

import java.net.URL;

import org.kalypsodeegree.graphics.sld.RemoteOWS;
import org.kalypsodeegree.xml.Marshallable;
import org.kalypsodeegree_impl.tools.Debug;
import org.kalypsodeegree_impl.tools.NetWorker;

/**
 * Since a layer is defined as a collection of potentially mixed-type features, the UserLayer element must provide the
 * means to identify the features to be used. All features to be rendered are assumed to be fetched from a Web Feature
 * Server (WFS) or a Web Coverage Service (WCS, in which case the term features is used loosely).
 * <p>
 * </p>
 * The remote server to be used is identified by RemoteOWS (OGC Web Service) element.
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
class RemoteOWS_Impl implements RemoteOWS, Marshallable
{
  private String service = null;

  private URL onlineResource = null;

  /**
   * Creates a new RemoteOWS_Impl object.
   * 
   * @param service
   * @param onlineResource
   */
  RemoteOWS_Impl( String service, URL onlineResource )
  {
    setService( service );
    setOnlineResource( onlineResource );
  }

  /**
   * type of service that is represented by the remote ows. at the moment <tt>WFS</tt> and <tt>WCS</tt> are possible
   * values.
   * 
   * @return the type of the services
   */
  public String getService()
  {
    return service;
  }

  /**
   * @see RemoteOWS_Impl#getService()
   * @param service
   *          the type of the services
   */
  public void setService( String service )
  {
    this.service = service;
  }

  /**
   * address of the the ows as URL
   * 
   * @return the adress of the ows as URL
   */
  public URL getOnlineResource()
  {
    return onlineResource;
  }

  /**
   * @see RemoteOWS_Impl#getOnlineResource()
   * @param onlineResource
   *          the adress of the ows as URL
   */
  public void setOnlineResource( URL onlineResource )
  {
    this.onlineResource = onlineResource;
  }

  /**
   * exports the content of the RemoteOWS as XML formated String
   * 
   * @return xml representation of the RemoteOWS
   */
  public String exportAsXML()
  {
    Debug.debugMethodBegin();

    StringBuffer sb = new StringBuffer( 200 );
    sb.append( "<RemoteOWS>" );
    sb.append( "<Service>" ).append( service ).append( "</Service>" );
    sb.append( "<OnlineResource xmlns:xlink='http://www.w3.org/1999/xlink' " );
    sb.append( "xlink:type='simple' xlink:href='" );
    sb.append( NetWorker.url2String( onlineResource ) + "'/>" );
    sb.append( "</RemoteOWS>" );

    Debug.debugMethodEnd();
    return sb.toString();
  }
}