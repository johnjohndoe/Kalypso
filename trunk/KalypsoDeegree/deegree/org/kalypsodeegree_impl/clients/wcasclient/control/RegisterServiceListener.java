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
package org.deegree_impl.clients.wcasclient.control;

import java.net.URL;

import org.deegree.enterprise.control.FormEvent;
import org.deegree.enterprise.control.RPCException;
import org.deegree.enterprise.control.RPCMethodCall;
import org.deegree.enterprise.control.RPCParameter;
import org.deegree_impl.clients.wcasclient.configuration.CSWClientConfiguration;
import org.deegree_impl.enterprise.control.AbstractSecuredListener;
import org.deegree_impl.enterprise.control.RPCWebEvent;
import org.deegree_impl.tools.Debug;

/**
 * This <tt>Listener</tt> reacts on 'registerService'-events.
 * <p>
 * 
 * @author <a href="mschneider@lat-lon.de">Markus Schneider </a>
 */
public class RegisterServiceListener extends AbstractSecuredListener
{

  protected String serviceURL = null;

  protected String serviceType = null;

  protected URL catalogURL = null;

  public void performPrivilegedOperation( FormEvent event )
  {
    Debug.debugMethodBegin();

    try
    {
      // decode RPC-event
      if( event instanceof RPCWebEvent )
      {
        RPCWebEvent ev = (RPCWebEvent)event;
        RPCMethodCall rpcCall = ev.getRPCMethodCall();
        RPCParameter[] params = rpcCall.getParameters();

        if( params.length != 3 )
        {
          throw new RPCException( "Invalid RPC. Exactly three "
              + "'param'-elements below 'params' are required." );
        }
        for( int i = 0; i < 3; i++ )
        {
          if( !( params[i].getValue() instanceof String ) )
          {
            throw new RPCException( "Invalid RPC. 'param'-elements "
                + "below 'params' must contain string-values." );
          }
        }
        catalogURL = CSWClientConfiguration.getInstance().getCatalogServerAddress(
            (String)params[0].getValue() );
        serviceURL = (String)params[1].getValue();
        serviceType = (String)params[2].getValue();
      }
      else
      {
        throw new Exception( "Es wurde kein gültiger RPC-event empfangen." );
      }
      getRequest().setAttribute(
          "MESSAGE",
          "Der Service mit der URL <tt>http://" + serviceURL + "<tt>"
              + "</tt> wurde dem Catalog erfolgreich hinzugefügt." );
    }
    catch( Exception e )
    {
      getRequest().setAttribute( "SOURCE", this.getClass().getName() );
      getRequest().setAttribute(
          "MESSAGE",
          "Der Service konnte nicht hinzugefügt werden, " + "da ein Fehler augetreten ist.<br><br>"
              + "Die Fehlermeldung lautet: <code>" + e.getMessage() + "</code>" );
      setNextPage( "admin_error.jsp" );
    }

    Debug.debugMethodEnd();
  }
}