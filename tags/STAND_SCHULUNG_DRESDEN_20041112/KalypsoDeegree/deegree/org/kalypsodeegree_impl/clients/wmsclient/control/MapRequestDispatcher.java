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

package org.deegree_impl.clients.wmsclient.control;

import java.io.IOException;

import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * This is a <code>RequestDispatcher</code> which creates a event out of a GET
 * or POST requests.
 * <P>
 * 
 * Furthermore this class implements
 * 
 * <HR>
 * <B>Design Patterns: </B>: <BR>
 * 
 * The following Design Patterns are used:
 * <UL>
 * <LI>Proxy
 * </UL>
 * 
 * @author <a href="mailto:friebe@gmx.net">Torsten Friebe </a>
 * 
 * @version $Revision$ $Date$
 *  
 */
public class MapRequestDispatcher extends org.deegree_impl.enterprise.control.RequestDispatcher
{

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

    String controllerFile = getInitParameter( "Handler.configFile" );
    String clientConfig = this.getInitParameter( "MapClient.configFile" );
    try
    {
      this.appHandler = new MapApplicationHandler( controllerFile, clientConfig );
    }
    catch( Exception e )
    {
      getServletContext().log( e.toString() );
      System.exit( 1 );
    }

  }

  /**
   * 
   * 
   * @param request
   * @param response
   * 
   * @throws ServletException
   * @throws IOException
   */
  protected void service( HttpServletRequest request, HttpServletResponse response )
      throws ServletException, IOException
  {

    super.service( request, response );
  }

}

/*
 * Changes to this class. What the people haven been up to: $Log:
 * MapRequestDispatcher.java,v $ Revision 1.2 2004/01/03 13:46:45 poth no
 * message
 * 
 * Revision 1.1 2003/07/11 12:29:58 poth no message
 * 
 * Revision 1.1.1.1 2002/03/22 15:23:10 ap no message
 * 
 *  
 */
