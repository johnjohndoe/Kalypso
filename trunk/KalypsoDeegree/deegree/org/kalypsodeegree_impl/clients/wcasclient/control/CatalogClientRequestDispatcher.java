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

import java.io.IOException;
import java.net.URL;

import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.deegree.enterprise.control.FormEvent;
import org.deegree_impl.clients.wcasclient.configuration.CSWClientConfiguration;
import org.deegree_impl.tools.StringExtend;

/**
 * 
 * 
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 */
public class CatalogClientRequestDispatcher extends
    org.deegree_impl.enterprise.control.RequestDispatcher
{

  /**
   * Comment for <code>ERROR_PAGE</code>
   */
  public static String ERROR_PAGE = "error.jsp";

  /**
   * This method initializes the servlet.
   * 
   * @param cfg
   *          the servlet configuration
   * 
   * @throws ServletException
   *           an exception
   */
  public void init( ServletConfig cfg ) throws ServletException
  {
    super.init( cfg );

    try
    {
      // initialize configuration of client and data servers
      CSWClientConfiguration.getInstance( new URL( getInitParameter( "Client.configFile" ) ) );
    }
    catch( Exception e )
    {
      e.printStackTrace();
      throw new ServletException( StringExtend.stackTraceToString( e.getStackTrace() ) );
    }

    if( getInitParameter( "Client.errorPage" ) != null )
    {
      ERROR_PAGE = getInitParameter( "Client.errorPage" );
    }
  }

  protected void service( HttpServletRequest request, HttpServletResponse response )
      throws ServletException, IOException
  {

    // create event out of request
    FormEvent _event = createEvent( request );

    // deliver event to application handler
    deliverEvent( _event );

    // get next page from request attribute
    String nextPage = (String)request.getAttribute( "next" );

    // show error page if next page is null
    if( nextPage == null )
      nextPage = ERROR_PAGE;
    nextPage = "/" + nextPage;

    if( request.getAttribute( "javax.servlet.jsp.jspException" ) != null )
    {
      nextPage = "/" + ERROR_PAGE;
    }

    try
    {
      request.setAttribute( "CONFIGURATION", CSWClientConfiguration.getInstance() );
    }
    catch( Exception e )
    {
      System.out.println( e );
    }

    // call request dispatcher
    getServletConfig().getServletContext().getRequestDispatcher( nextPage ).forward( request,
        response );
    _event = null;
  }
}