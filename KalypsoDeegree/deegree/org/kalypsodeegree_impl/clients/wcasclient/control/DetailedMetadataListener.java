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
import org.deegree_impl.clients.wcasclient.Constants;
import org.deegree_impl.clients.wcasclient.configuration.CSWClientConfiguration;
import org.deegree_impl.clients.wcasclient.configuration.TextComponent;
import org.deegree_impl.enterprise.control.AbstractListener;
import org.deegree_impl.tools.Debug;


/**
 * Listener for accessing a full metadata description from the users session
 *
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @version $Revision$ $Date$
 */
public class DetailedMetadataListener extends AbstractListener {
    /**
     *
     *
     * @param e 
     */
    public void actionPerformed( FormEvent e ) {
        Debug.debugMethodBegin();

        // get Metadata (DetailedMetadata) from the users sesssion
        HttpSession session = ( (HttpServletRequest)this.getRequest() ).getSession( true );
        Object o = session.getAttribute( Constants.SESSION_METADATA );
        if ( o != null ) {
            getRequest().setAttribute( Constants.RESULT_SEARCH, o );
        } else {
            // create error message ifno metadata object is contained in the 
            // users session
            setNextPage( "error.jsp" );
            CSWClientConfiguration conf = CSWClientConfiguration.getInstance();
            TextComponent tc = conf.getTextComponent();
            try {
                getRequest().setAttribute( Constants.MESSAGE, 
                                           tc.getMessage( "search/noMetadata" ) );
            } catch (Exception ex) {
                ex.printStackTrace();
            }
            getRequest().setAttribute( Constants.SOURCE, this.getClass().getName() );
        }

        Debug.debugMethodEnd();
    }
}