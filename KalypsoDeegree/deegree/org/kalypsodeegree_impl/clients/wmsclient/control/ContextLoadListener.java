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

import java.io.File;
import java.util.ArrayList;

import javax.servlet.http.HttpServletRequest;

import org.deegree.enterprise.control.FormEvent;
import org.deegree.enterprise.control.RPCMember;
import org.deegree.enterprise.control.RPCMethodCall;
import org.deegree.enterprise.control.RPCParameter;
import org.deegree.enterprise.control.RPCStruct;
import org.deegree_impl.clients.ClientException;
import org.deegree_impl.enterprise.control.AbstractListener;
import org.deegree_impl.enterprise.control.RPCWebEvent;
import org.deegree_impl.tools.Debug;

/**
 * ...under development use at own risk ;-)
 * 
 * TODO: sort out user issues (where, how, what) currently only .xml files are
 * being accepted as contexts also assuming und WEB-INF/xml/users/ user
 * directories TODO write doc
 * 
 * @author <a href="mailto:taddei@lat-lon.de">Ugo Taddei </a>
 *  
 */
public class ContextLoadListener extends AbstractListener
{

  /**
   * @see org.deegree.enterprise.control.WebListener#actionPerformed(org.deegree.enterprise.control.FormEvent)
   */
  public void actionPerformed( FormEvent event )
  {

    Debug.debugMethodBegin( this, "actionPerformed" );

    RPCWebEvent rpc = (RPCWebEvent)event;
    try
    {
      validate( rpc );
    }
    catch( ClientException e )
    {
      gotoErrorPage( "Not a valid RPC for ContextLoad\n" + e.getMessage() );
    }

    RPCMethodCall mc = ( (RPCWebEvent)event ).getRPCMethodCall();
    RPCParameter[] pars = mc.getParameters();
    RPCStruct struct = (RPCStruct)pars[0].getValue();

    // get map context value
    String username = (String)struct.getMember( "username" ).getValue();

    String path2Dir = ( (HttpServletRequest)this.getRequest() ).getSession( true )
        .getServletContext().getRealPath( "/" )
        + "WEB-INF/xml/users/" + username;

    File dir = new File( path2Dir );
    File[] files = dir.listFiles();
    ArrayList contextList = new ArrayList();
    for( int i = 0; i < files.length; i++ )
    {
      String s = files[i].getName();
      if( files[i].isFile() && s.endsWith( ".xml" ) )
      {
        contextList.add( files[i].getName() );
      }
    }
    getRequest().setAttribute( "CONTEXT_LIST", contextList );
    getRequest().setAttribute( "USER", username );

    Debug.debugMethodEnd();
  }

  private void validate( RPCWebEvent rpc ) throws ClientException
  {
    RPCMethodCall mc = rpc.getRPCMethodCall();
    RPCParameter param = mc.getParameters()[0];
    RPCStruct struct = (RPCStruct)param.getValue();
    RPCMember username = struct.getMember( "username" );
    if( username == null )
    {
      throw new ClientException( "missing parameter 'username' in RPC for ContextSave" );
    }
  }

}