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

// $Id$
package org.deegree_impl.enterprise.control;

// JDK 1.3
import java.util.Enumeration;
import java.util.EventObject;
import java.util.Properties;

import javax.servlet.http.HttpServletRequest;

import org.deegree.enterprise.control.FormEvent;
import org.deegree.enterprise.control.RequestUser;

/**
 * 
 * @author <a href="mailto:tfriebe@gmx.net">Torsten Friebe </a>
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * 
 * @version $Revision$
 */
public class WebEvent extends EventObject implements FormEvent
{
  /**
   * Creates a new WebEvent object.
   * 
   * @param request
   */
  public WebEvent( HttpServletRequest request )
  {
    super( request );
  }

  /**
   * 
   * 
   * @return
   */
  public Properties getParameter()
  {
    return this._getParameters( this._getRequest() );
  }

  /**
   * 
   * 
   * @return
   */
  public String getDocumentPath()
  {
    return this._getRequest().getRequestURI();
  }

  /**
   * 
   * 
   * @return
   */
  public RequestUser getRequestUser()
  {
    return this._getRequestUser( this._getRequest() );
  }

  /**
   * 
   * 
   * @return
   */
  public String toString()
  {
    return new String( this.getClass().getName() + " [ " + getDocumentPath() + " ]" );
  }

  /**
   * Returns a list of Properties with key value pairs created out of the
   * incoming POST paramteres.
   */
  private Properties _getParameters( HttpServletRequest request )
  {
    Properties p = new Properties();

    for( Enumeration e = request.getParameterNames(); e.hasMoreElements(); )
    {
      String key = (String)e.nextElement();
      p.setProperty( key, request.getParameter( key ) );
    }

    return p;
  }

  /**
   * 
   * 
   * @param request
   * 
   * @return
   */
  private RequestUser _getRequestUser( HttpServletRequest request )
  {
    return new RequestUser_Impl( request );
  }

  /**
   * 
   * 
   * @return
   */
  private HttpServletRequest _getRequest()
  {
    return (HttpServletRequest)getSource();
  }

  /** @link dependency */

  /* #RequestUser lnkRequestUser; */
}