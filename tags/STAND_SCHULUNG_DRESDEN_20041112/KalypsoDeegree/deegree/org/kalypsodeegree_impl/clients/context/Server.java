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
package org.deegree_impl.clients.context;

import java.net.URL;

import org.deegree.xml.Marshallable;
import org.deegree_impl.tools.NetWorker;

/**
 * encapsulates the server description as defined in the OGC Web Map Context
 * specification 1.0.0
 * 
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 */
public class Server implements Marshallable
{
  private String service = null;

  private String title = null;

  private String version = null;

  private URL onlineResource = null;

  /**
   * Creates a new Server object.
   * 
   * @param title
   *          the title of the service
   * @param version
   *          Version number of the OGC interface specification which
   *          corresponds to the service
   * @param service
   *          the type of the service according to OGC interfaces, such as WMS,
   *          WFS.
   * @param onlineResource
   *          the link to the online resource
   * 
   * @throws ContextException
   */
  public Server( String title, String version, String service, URL onlineResource )
      throws ContextException
  {
    setTitle( title );
    setVersion( version );
    setService( service );
    setOnlineResource( onlineResource );
  }

  /**
   * the title of the service (extracted from the Capabilities by the Context
   * document creator)
   * 
   * @return
   */
  public String getTitle()
  {
    return title;
  }

  /**
   * Version number of the OGC interface specification which corresponds to the
   * service
   * 
   * @return
   */
  public String getVersion()
  {
    return version;
  }

  /**
   * the type of the service according to OGC interfaces, such as WMS, WFS.
   * 
   * @return
   */
  public String getService()
  {
    return service;
  }

  /**
   * link to the online resource
   * 
   * @return
   */
  public URL getOnlineResource()
  {
    return onlineResource;
  }

  /**
   * @see org.deegree_impl.clients.context.Server#getTitle()
   * 
   * @param title
   */
  public void setTitle( String title )
  {
    this.title = title;
  }

  /**
   * @see org.deegree_impl.clients.context.Server#getVersion()
   * 
   * @param version
   * 
   * @throws ContextException
   */
  public void setVersion( String version ) throws ContextException
  {
    if( version == null )
    {
      throw new ContextException( "version isn't allowed to be null" );
    }
    this.version = version;
  }

  /**
   * @see org.deegree_impl.clients.context.Server#getService()
   * 
   * @param service
   * 
   * @throws ContextException
   */
  public void setService( String service ) throws ContextException
  {
    if( service == null )
    {
      throw new ContextException( "service isn't allowed to be null" );
    }
    this.service = service;
  }

  /**
   * @see org.deegree_impl.clients.context.Server#getOnlineResource()
   * 
   * @param onlineResource
   * 
   * @throws ContextException
   */
  public void setOnlineResource( URL onlineResource ) throws ContextException
  {
    if( onlineResource == null )
    {
      throw new ContextException( "onlineResource isn't allowed to be null" );
    }
    this.onlineResource = onlineResource;
  }

  /**
   * @see org.deegree.xml.Marshallable#exportAsXML()
   */
  public String exportAsXML()
  {

    StringBuffer sb = new StringBuffer( 1000 );
    sb.append( "<Server " );
    if( title != null )
    {
      sb.append( "title='" ).append( title ).append( "' " );
    }
    sb.append( "service='" ).append( service ).append( "' version='" ).append( version ).append(
        "'>" );
    sb.append( "<OnlineResource  xlink:type='simple' xlink:href='" ).append(
        NetWorker.url2String( onlineResource ) ).append( "'/>" );
    sb.append( "</Server>" );
    return sb.toString();
  }

}