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

// JSDK 2.2
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import org.deegree.enterprise.control.FormEvent;
import org.deegree.enterprise.control.RPCMethodCall;
import org.deegree.enterprise.control.RPCParameter;
import org.deegree.enterprise.control.RPCStruct;
import org.deegree_impl.clients.wcasclient.Constants;
import org.deegree_impl.clients.wcasclient.model.Selection;
import org.deegree_impl.enterprise.control.AbstractListener;
import org.deegree_impl.enterprise.control.RPCWebEvent;
import org.deegree_impl.tools.Debug;


/**
 * deletes an entry from the selection/shoppingcard of a user. the required 
 * informations are contained in the RPCWebEvent passed to the actionPerformed method.
 *
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @version $Revision$ $Date$
 */
public class DeleteFromShoppingCartListener extends AbstractListener {
    /**
     *
     *
     * @param e 
     */
    public void actionPerformed( FormEvent e ) {
        Debug.debugMethodBegin();        

        // get/create the Session
        HttpSession session = ( (HttpServletRequest)this.getRequest() ).getSession( true );
        // get the selection from the users sesssion
        Selection sel = (Selection)session.getAttribute( Constants.SESSION_SELECTION );

        if ( sel != null ) {    
            RPCMethodCall mc = ((RPCWebEvent)e).getRPCMethodCall();
            RPCParameter[] params = mc.getParameters();
            for (int i = 0; i < params.length; i++) {
                RPCStruct struct = (RPCStruct)params[i].getValue();
                String catalog = (String)struct.getMember( Constants.RPC_CATALOG ).getValue();
                // get parameter array containing the fids to be deleted from 
                // the shopping card
                String fileIdentifier = 
                    (String)struct.getMember( Constants.RPC_IDENTIFIER ).getValue();                          
                sel.removeSelectionEntry( fileIdentifier, catalog );
            }
            // write the selection (back) to the users session
            session.setAttribute( Constants.SESSION_SELECTION, sel );
            getRequest().setAttribute( Constants.SESSION_SELECTION, sel );
        }
        
        Debug.debugMethodEnd();
    }

   
}