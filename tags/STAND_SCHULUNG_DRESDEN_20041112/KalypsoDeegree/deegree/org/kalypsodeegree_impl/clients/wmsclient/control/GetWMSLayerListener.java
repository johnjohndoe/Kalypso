// $Header:
// /cvsroot/deegree/deegree/org/deegree_impl/clients/wmsclient/control/GetWMSLayerListener.java,v
// 1.3 2004/07/30 06:57:22 poth Exp $
/*----------------    FILE HEADER  ------------------------------------------

 This file is part of deegree.
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon GmbH
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
 lat/lon GmbH
 Meckenheimer Allee 176
 53115 Bonn
 Germany
 E-Mail: poth@lat-lon.de

 Klaus Greve
 Department of Geography
 University of Bonn
 Meckenheimer Allee 166
 53115 Bonn
 Germany
 E-Mail: klaus.greve@uni-bonn.de

 
 ---------------------------------------------------------------------------*/
package org.deegree_impl.clients.wmsclient.control;

import java.net.MalformedURLException;
import java.net.URL;

import org.deegree.enterprise.control.FormEvent;
import org.deegree.enterprise.control.RPCMember;
import org.deegree.enterprise.control.RPCMethodCall;
import org.deegree.enterprise.control.RPCParameter;
import org.deegree.enterprise.control.RPCStruct;
import org.deegree.services.wms.capabilities.WMSCapabilities;
import org.deegree.xml.XMLParsingException;
import org.deegree_impl.clients.ClientException;
import org.deegree_impl.clients.wmsclient.tools.ClientHelper;
import org.deegree_impl.enterprise.control.AbstractListener;
import org.deegree_impl.enterprise.control.RPCWebEvent;
import org.deegree_impl.services.wms.capabilities.OGCWMSCapabilitiesFactory;
import org.deegree_impl.tools.NetWorker;
import org.deegree_impl.tools.StringExtend;

/**
 * Lister class for accessing the layers of WMS and return it to the client.
 * 
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @author last edited by: $Author$
 * 
 * @version 1.0. $Revision$, $Date$
 * 
 * @since 2.0
 */
public class GetWMSLayerListener extends AbstractListener
{

  /**
   * @see org.deegree.enterprise.control.WebListener#actionPerformed(org.deegree.enterprise.control.FormEvent)
   */
  public void actionPerformed( FormEvent event )
  {

    RPCWebEvent rpc = (RPCWebEvent)event;
    try
    {
      validate( rpc );
    }
    catch( Exception e )
    {
      gotoErrorPage( "Not a valid RPC for GetWMSLayer\n" + e.getMessage() );
    }

    WMSCapabilities capabilities = null;
    URL url = null;
    try
    {
      url = getURL( rpc );
      capabilities = getWMSCapabilities( url );
    }
    catch( MalformedURLException ue )
    {
      gotoErrorPage( "Not a valid URL for a WMS in GetWMSLayer\n" + ue.getMessage() );
    }
    catch( XMLParsingException xe )
    {
      gotoErrorPage( "Could not parse capabilities coming from the WMS " + "\n"
          + StringExtend.stackTraceToString( xe ) );
    }

    String s = ClientHelper.getLayersAsTree( capabilities.getCapability().getLayer() );
    getRequest().setAttribute( "WMSLAYER", s );
    getRequest().setAttribute( "WMSURL", NetWorker.url2String( url ) );
    getRequest().setAttribute( "WMSVERSION", getVersion( rpc ) );
    s = capabilities.getService().getTitle();
    s = s.replaceAll( "'", "" );
    getRequest().setAttribute( "WMSNAME", s );
  }

  private void validate( RPCWebEvent rpc ) throws ClientException
  {
    RPCMethodCall mc = rpc.getRPCMethodCall();
    RPCParameter param = mc.getParameters()[0];
    RPCStruct struct = (RPCStruct)param.getValue();
    RPCMember address = struct.getMember( "WMSURL" );
    if( address == null )
    {
      throw new ClientException( "missing parameter WMSURL in RPC for GetWMSLayer" );
    }
    RPCMember version = struct.getMember( "VERSION" );
    if( version == null )
    {
      throw new ClientException( "missing parameter VERSION in RPC for GetWMSLayer" );
    }
    if( ( (String)version.getValue() ).compareTo( "1.1.0" ) < 0 )
    {
      throw new ClientException( "VERSION must be >= 1.1.0" );
    }
  }

  private String getVersion( RPCWebEvent rpc )
  {
    RPCMethodCall mc = rpc.getRPCMethodCall();
    RPCParameter param = mc.getParameters()[0];
    RPCStruct struct = (RPCStruct)param.getValue();
    RPCMember version = struct.getMember( "VERSION" );
    return (String)version.getValue();
  }

  private URL getURL( RPCWebEvent rpc ) throws MalformedURLException
  {
    RPCMethodCall mc = rpc.getRPCMethodCall();
    RPCParameter param = mc.getParameters()[0];
    RPCStruct struct = (RPCStruct)param.getValue();
    RPCMember version = struct.getMember( "VERSION" );
    RPCMember address = struct.getMember( "WMSURL" );
    StringBuffer sb = new StringBuffer( (String)address.getValue() );
    sb.append( "?service=WMS&request=GetCapabilities&" );
    sb.append( "version=" ).append( (String)version.getValue() );
    return new URL( sb.toString() );
  }

  private WMSCapabilities getWMSCapabilities( URL url ) throws XMLParsingException
  {

    OGCWMSCapabilitiesFactory fac = new OGCWMSCapabilitiesFactory();
    WMSCapabilities capabilities = fac.createCapabilities( url );

    return capabilities;
  }
}

/*******************************************************************************
 * Changes to this class. What the people have been up to: $Log:
 * GetWMSLayerListener.java,v $ Revision 1.3 2004/07/30 06:57:22 poth no message
 * 
 * Revision 1.2 2004/07/21 07:43:42 poth no message
 * 
 * Revision 1.1 2004/07/19 07:41:42 poth no message
 * 
 *  
 ******************************************************************************/