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
 * This class encapsulates a reference to a Web Map Context document as defined
 * int the OGC Web Map Context specification
 *
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 */
public class ViewContextReference implements Marshallable {
    private String title = null;
    private URL contextURL = null;

    /**
     * Creates a new ViewContextReference object.
     *
     * @param title title of the context
     * @param contextURL URL where to access the context
     *
     * @throws ContextException 
     */
    public ViewContextReference( String title, URL contextURL ) throws ContextException {
        setTitle( title );
        setContextURL( contextURL );
    }

    /**
     *
     *
     * @return 
     */
    public String getTitle() {
        return title;
    }

    /**
     *
     *
     * @param title 
     *
     * @throws ContextException 
     */
    public void setTitle( String title ) throws ContextException {
        if ( title == null ) {
            throw new ContextException( "title isn't allowed to be null" );
        }

        this.title = title;
    }

    /**
     *
     *
     * @return 
     */
    public URL getContextURL() {
        return contextURL;
    }

    /**
     *
     *
     * @param contextURL 
     *
     * @throws ContextException 
     */
    public void setContextURL( URL contextURL ) throws ContextException {
        if ( contextURL == null ) {
            throw new ContextException( "contextURL isn't allowed to be null" );
        }

        this.contextURL = contextURL;
    }

    /**
     *
     *
     * @return 
     */
    public String exportAsXML() {
        StringBuffer sb = new StringBuffer( 500 );
        sb.append( "<ViewContextReference>" );
        sb.append( "<Title>" ).append( title ).append( "</Title>" );
        sb.append( "<ContextURL>" );
        sb.append( "<OnlineResource  xlink:type='simple' xlink:href='" )
          .append( NetWorker.url2String( contextURL ) ).append( "'/>" );
        sb.append( "</ContextURL>" );
        sb.append( "</ViewContextReference>" );
        return sb.toString();
    }
}