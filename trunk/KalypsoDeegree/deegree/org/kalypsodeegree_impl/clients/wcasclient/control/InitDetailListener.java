// $Header:
// /cvsroot/deegree/deegree/org/deegree_impl/clients/wcasclient/control/InitDetailListener.java,v
// 1.11 2004/04/16 07:15:20 poth Exp $
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

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import org.deegree.enterprise.control.FormEvent;
import org.deegree.enterprise.control.RPCException;
import org.deegree.enterprise.control.RPCMethodResponse;
import org.deegree.enterprise.control.RPCParameter;
import org.deegree_impl.clients.wcasclient.CatalogClientException;
import org.deegree_impl.clients.wcasclient.Constants;
import org.deegree_impl.enterprise.control.AbstractListener;
import org.deegree_impl.enterprise.control.RPCFactory;
import org.deegree_impl.enterprise.control.RPCWebEvent;
import org.deegree_impl.tools.Debug;

/**
 * 
 * 
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @author last edited by: $Author$
 * 
 * @version 1.0. $Revision$, $Date$
 * 
 * @since 1.1
 */
public class InitDetailListener extends AbstractListener
{
  /**
   *  
   */
  public void actionPerformed( FormEvent event )
  {
    Debug.debugMethodBegin( this, "actionPerformed" );

    // remove result from a previous search (full metadata description)
    // from the session
    HttpSession session = ( (HttpServletRequest)this.getRequest() ).getSession( true );
    session.removeAttribute( Constants.SESSION_METADATA );

    RPCWebEvent rpcEvent = (RPCWebEvent)event;

    try
    {
      validateRequest( rpcEvent );
    }
    catch( Exception e )
    {
      gotoErrorPage( "Invalid Request: " + e.toString() );
      Debug.debugMethodEnd();
      return;
    }

    try
    {
      handle( rpcEvent );
    }
    catch( Exception e )
    {
      gotoErrorPage( "Error handling result: " + e.toString() );
      Debug.debugMethodEnd();
      return;
    }

    Debug.debugMethodEnd();
  }

  /**
   * validates the request to be performed.
   * 
   * @param event
   *          event object containing the request to be performed
   */
  protected void validateRequest( RPCWebEvent event ) throws CatalogClientException
  {
    Debug.debugMethodBegin();

    // TODO
    Debug.debugMethodEnd();
  }

  /**
   * handles the result of a 'FULL' catalog query
   * 
   * @param event
   *          result to a GetRecord request
   */
  protected void handle( RPCWebEvent event ) throws CatalogClientException
  {
    Debug.debugMethodBegin();

    HttpSession session = ( (HttpServletRequest)this.getRequest() ).getSession( true );
    RPCParameter[] detailedsearchParams = (RPCParameter[])session
        .getAttribute( Constants.SESSION_DETAILEDSEARCHPARAM );

    if( detailedsearchParams != null )
    {
      try
      {
        RPCMethodResponse resp = RPCFactory.createRPCMethodResponse( detailedsearchParams );
        this.getRequest().setAttribute( Constants.SESSION_DETAILEDSEARCHPARAM, resp );
      }
      catch( RPCException e )
      {
        e.printStackTrace();
      }
    }

    Debug.debugMethodEnd();
  }
}
/*******************************************************************************
 * Changes to this class. What the people have been up to: $Log:
 * InitDetailListener.java,v $ Revision 1.11 2004/04/16 07:15:20 poth no message
 * 
 * 
 *  
 ******************************************************************************/