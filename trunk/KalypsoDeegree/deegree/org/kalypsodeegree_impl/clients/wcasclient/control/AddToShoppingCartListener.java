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

import org.deegree.enterprise.control.*;
import org.deegree.model.geometry.GM_Envelope;

import org.deegree_impl.clients.wcasclient.Constants;
import org.deegree_impl.clients.wcasclient.model.Selection;
import org.deegree_impl.clients.wcasclient.model.SelectionEntry;
import org.deegree_impl.enterprise.control.*;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.deegree_impl.tools.Debug;
import org.deegree_impl.tools.StringExtend;


/**
 * adds an entray to the selection/shoppingcard of a user. the required informations
 * are contained in the RPCWebEvent passed to the actionPerformed method.
 *
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @version $Revision$ $Date$
 */
public class AddToShoppingCartListener extends AbstractListener {
    
    /**
     *
     *
     * @param e 
     */
    public void actionPerformed( FormEvent e ) {
        Debug.debugMethodBegin();

        SelectionEntry selE = createSelectionEntry( (RPCWebEvent)e );

        // get/create the Session
        HttpSession session = ( (HttpServletRequest)this.getRequest() ).getSession( true );
        // get the selection from the users sesssion
        Selection sel = (Selection)session.getAttribute( Constants.SESSION_SELECTION );

        // create a new selection if this user hasn't already one
        if ( sel == null ) {
            sel = new Selection();
        }

        // add the new entry to the selection
        sel.addEntry( selE );

        // write the selection (back) to the users session
        session.setAttribute( Constants.SESSION_SELECTION, sel );
        getRequest().setAttribute( Constants.MESSAGE, "Der Datensatz wurde der Auswahl hinzugef&uuml;gt");
        
        Debug.debugMethodEnd();
    }

    /**
     * creates a <tt>SelectionEntry</tt> from the incomming <tt>RPCWebEvent</tt>
     * and the contained data/parameters.
     *
     * @param we 
     *
     * @return SelectionEntry
     */
    private SelectionEntry createSelectionEntry( RPCWebEvent we ) {
        Debug.debugMethodBegin();

        RPCParameter[] param = we.getRPCMethodCall().getParameters();
        RPCStruct struct = (RPCStruct)param[0].getValue();
        String id = (String)struct.getMember( "ID" ).getValue();
        String title = (String)struct.getMember( "title" ).getValue();
        String catalog = (String)struct.getMember( Constants.RPC_CATALOG ).getValue();
        RPCStruct bboxStruct = (RPCStruct)struct.getMember( Constants.RPC_BBOX ).getValue();
        Double minx = (Double)bboxStruct.getMember( Constants.RPC_BBOXMINX ).getValue();
        Double miny = (Double)bboxStruct.getMember( Constants.RPC_BBOXMINY ).getValue();
        Double maxx = (Double)bboxStruct.getMember( Constants.RPC_BBOXMAXX ).getValue();
        Double maxy = (Double)bboxStruct.getMember( Constants.RPC_BBOXMAXY ).getValue();

        GM_Envelope bbox = 
        		GeometryFactory.createGM_Envelope( minx.doubleValue(), miny.doubleValue(), 
                                                   maxx.doubleValue(), maxy.doubleValue() );
        String services = (String)struct.getMember( Constants.RPC_AVAILABILITY ).getValue();
        String[] tmp = StringExtend.toArray( services, ",", true );
        int[] availabilitiy = new int[ tmp.length ];
        for (int i = 0; i < tmp.length; i++) {
            if ( tmp[i].trim().equals( "WMS" ) ) {
                availabilitiy[i] = SelectionEntry.VIEW;
            } else if ( tmp[i].trim().equals( "WFS" ) ) {
                availabilitiy[i] = SelectionEntry.DOWNLOAD;
            } else {
                availabilitiy[i] = SelectionEntry.UNKNOWN;
            }
        }

        SelectionEntry sel = new SelectionEntry( id, title, catalog, bbox, availabilitiy );

        Debug.debugMethodEnd();
        return sel;
    }
}