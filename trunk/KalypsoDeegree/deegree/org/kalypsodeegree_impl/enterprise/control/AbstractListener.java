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
package org.deegree_impl.enterprise.control;

import java.util.Enumeration;
import java.util.HashMap;

import javax.servlet.ServletRequest;

import org.deegree.enterprise.control.FormEvent;
import org.deegree.enterprise.control.WebListener;
import org.deegree_impl.tools.StringExtend;

/**
 * The abstract listener allows the reuse of basic functionality.
 * 
 * @author <a href="mailto:tfriebe@gmx.net">Torsten Friebe </a>
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * 
 * @version $Revision$
 */
public abstract class AbstractListener implements WebListener
{
  private FormEvent event;

  private Object returnValue;

  private String alternativeDefaultTarget;

  private String alternativeNext;

  private String defaultTarget;

  private String next;

  /**
   * 
   * 
   * @param e
   */
  public abstract void actionPerformed( FormEvent e );

  /**
   * 
   * 
   * @param e
   */
  public final void handle( FormEvent e )
  {
    this.event = e;
    this.getNextPageFormRequest();
    this.actionPerformed( e );
    getRequest().setAttribute( "returnValue", getReturnValue() );
    getRequest().setAttribute( "next", getNextPage() );
  }

  /**
   * 
   * 
   * @return
   */
  public ServletRequest getRequest()
  {
    Object source = this.event.getSource();
    return (ServletRequest)source;
  }

  /**
   * 
   * 
   * @param target
   */
  protected final void setDefaultNextPage( String target )
  {
    this.defaultTarget = target;
  }

  /**
   * 
   * 
   * @param target
   */
  protected final void setDefaultAlternativeNextPage( String target )
  {
    this.alternativeDefaultTarget = target;
  }

  /**
   * Sets the next page for this request.
   */
  public void setNextPage( String target )
  {
    this.next = target;
  }

  /**
   * 
   * 
   * @return
   */
  public String getNextPage()
  {
    return ( ( this.next == null ) ? this.defaultTarget : this.next );
  }

  /**
   * 
   * 
   * @param target
   */
  public void setAlternativeNextPage( String target )
  {
    this.alternativeNext = target;
  }

  /**
   * 
   * 
   * @return
   */
  public String getAlternativeNextPage()
  {
    return ( ( this.alternativeNext == null ) ? this.alternativeDefaultTarget
        : this.alternativeNext );
  }

  /**
   * 
   * 
   * @return
   */
  public Object getReturnValue()
  {
    return this.returnValue;
  }

  /**
   * 
   * 
   * @param model
   */
  public void setReturnValue( Object model )
  {
    this.returnValue = model;
  }

  /**
   *  
   */
  private void getNextPageFormRequest()
  {
    String target = null;

    if( ( target = this.getRequest().getParameter( "nextPage" ) ) != null )
    {
      this.setNextPage( target );
    }
  }

  /**
   *  
   */
  protected void gotoErrorPage( String message )
  {
    getRequest().setAttribute( "SOURCE", "" + this.getClass().getName() );
    getRequest().setAttribute( "MESSAGE", message );
    setNextPage( "error.jsp" );
  }

  /**
   * transforms the request to a set of name value pairs stored in a HashMap
   */
  protected HashMap toModel()
  {
    HashMap model = new HashMap();
    ServletRequest req = getRequest();
    Enumeration enum = req.getParameterNames();

    while( enum.hasMoreElements() )
    {
      String name = (String)enum.nextElement();
      String[] value = req.getParameterValues( name );
      int pos = name.indexOf( '@' ) + 1;
      if( pos < 0 )
      {
        pos = 0;
      }
      name = name.substring( pos, name.length() );
      model.put( name.toUpperCase(), StringExtend.arrayToString( value, ',' ) );
    }

    String user = event.getRequestUser().getRemoteUser();
    int k = user.lastIndexOf( "\\" );
    if( k > 0 )
    {
      user = user.substring( k + 1 );
    }
    model.put( "USERNAME", user );
    model.put( "USERNAME", "SEC_ADMIN" );
    return model;
  }
}