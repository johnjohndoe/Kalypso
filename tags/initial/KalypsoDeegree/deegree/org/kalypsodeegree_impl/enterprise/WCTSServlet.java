/*----------------    FILE HEADER  ------------------------------------------
 
This file is part of JaGo (Java Framework for Geospatial Solutions).
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
 
Markus Mueller
Department of Geography
University of Bonn
Meckenheimer Allee 166
53115 Bonn
Germany
E-Mail: mm@giub.uni-bonn.de
 
 
 ---------------------------------------------------------------------------*/

package org.deegree_impl.enterprise;

import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.deegree_impl.services.wcts.WCTService;
import org.deegree_impl.tools.Debug;


/**
 *
 * <p>--------------------------------------------------------</p>
 *
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @version $Revision$ $Date$
 */
public class WCTSServlet extends HttpServlet {
	
   
    /**
     * init-method of the servlet. only once called
     * @param servletConfig servlet configuration
     * @throws ServletException exception
     */
    public void init(ServletConfig servletConfig) throws ServletException {
        super.init(servletConfig);
        
        Debug.setLevel( getInitParameter("debug") );              
        
        
        try {
            //Debug.out = new PrintStream( new FileOutputStream("d:/log.txt") );
        } catch (Exception ex) {
            System.out.println(ex);
        }
                
    }   
    
    /**     
     * performs a http-post request
     */
    public void doPost(HttpServletRequest request, HttpServletResponse response) {

        Debug.debugMethodBegin( this, "doPost");

		new WCTService( request, response );
        
        Debug.debugMethodEnd();
    }
    
    
   /**     
     */
    public void doGet(HttpServletRequest request, HttpServletResponse response) {
    	Debug.debugMethodBegin( this, "doGet");
    	doPost( request, response );
    	Debug.debugMethodEnd();
    } 
    
    
}
