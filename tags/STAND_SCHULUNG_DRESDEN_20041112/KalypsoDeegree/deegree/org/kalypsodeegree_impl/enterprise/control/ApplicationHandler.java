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

// JDK 1.3
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLDecoder;
import java.util.HashMap;

import javax.servlet.ServletRequest;

import org.deegree.enterprise.control.FormEvent;
import org.deegree.enterprise.control.RPCException;
import org.deegree.enterprise.control.RPCMethodCall;
import org.deegree.enterprise.control.WebListener;
import org.deegree.xml.XMLTools;
import org.deegree_impl.tools.Debug;
import org.w3c.dom.Document;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

/**
 * Handler for all web events.
 * 
 * @author <a href="mailto:tfriebe@gmx.net">Torsten Friebe </a>
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * 
 * @version $Revision$
 *  
 */
public class ApplicationHandler implements WebListener
{
  private static final HashMap handler = new HashMap();

  private static final HashMap handlerNext = new HashMap();

  private static final HashMap handlerANext = new HashMap();

  private static final String EVENT = "event";

  private static final String NAME = "name";

  private static final String CLASS = "class";

  private static final String NEXT = "next";

  private static final String ALTERNATIVENEXT = "alternativeNext";

  /**
   * Creates a new ApplicationHandler object.
   * 
   * @param configFile
   */
  public ApplicationHandler( String configFile ) throws Exception
  {
    ApplicationHandler.initHandler( configFile );
  }

  /**
   * Handles all web action events. Calls the specified listener using the
   * mapping defined in control.xml file.
   * 
   * @param e
   *          the action event generated out of the incoming http POST event.
   */
  public void actionPerformed( FormEvent e )
  {
    Object source = e.getSource();

    if( source instanceof ServletRequest )
    {
      ServletRequest request = (ServletRequest)source;

      String actionName = request.getParameter( "action" );

      if( actionName != null )
      {
        // handle simple KVP encoded request
        try
        {
          if( "version".equalsIgnoreCase( actionName ) )
          {
            this.showVersion( request );
          }
          else
          {
            try
            {
              this.delegateToHelper( actionName, e );
            }
            catch( Exception ex )
            {
              ex.printStackTrace();
              System.out.println( "Action " + actionName + " is unknown!" );
            }
          }
        }
        catch( Exception ex )
        {
          request.setAttribute( "next", "error.jsp" );
          request.setAttribute( "javax.servlet.jsp.jspException", ex );
        }
      }
      else
      {
        // handle RPC encoded request
        try
        {
          RPCMethodCall mc = getMethodCall( request );
          e = new RPCWebEvent( e, mc );
          this.delegateToHelper( mc.getMethodName(), e );
        }
        catch( RPCException re )
        {
          re.printStackTrace();
          request.setAttribute( "next", "error.jsp" );
          request.setAttribute( "javax.servlet.jsp.jspException", re );
        }
        catch( Exception ee )
        {
          ee.printStackTrace();
          request.setAttribute( "next", "error.jsp" );
          request.setAttribute( "javax.servlet.jsp.jspException", ee );
        }
      }
    }
  }

  /**
   * extracts the RPC method call from the
   */
  private RPCMethodCall getMethodCall( ServletRequest request ) throws RPCException
  {
    Debug.debugMethodBegin( this, "getMethodCall" );

    String s = request.getParameter( "rpc" );
    try
    {
      if( s == null )
      {
        StringBuffer sb = new StringBuffer( 1000 );
        try
        {
          BufferedReader br = request.getReader();
          String line = null;
          while( ( line = br.readLine() ) != null )
          {
            sb.append( line );
          }
          br.close();
        }
        catch( Exception e )
        {
          throw new RPCException( "Error reading stream from servlet\n" + e.toString() );
        }

        s = sb.toString();
        s = URLDecoder.decode( s, "UTF-8" );
        int pos1 = s.indexOf( "<methodCall>" );
        int pos2 = s.indexOf( "</methodCall>" );
        if( pos1 < 0 )
        {
          throw new RPCException( "request doesn't contain a RPC methodCall" );
        }
        s = s.substring( pos1, pos2 + 13 );
      }
      else
      {
        s = URLDecoder.decode( s, "UTF-8" );
      }
    }
    catch( Exception e )
    {
      throw new RPCException( e.toString() );
    }

    StringReader reader = new StringReader( s );
    RPCMethodCall mc = RPCFactory.createRPCMethodCall( reader );

    Debug.debugMethodEnd();
    return mc;
  }

  /**
   * 
   * 
   * @param action
   * @param e
   * 
   * @throws Exception
   */
  protected void delegateToHelper( String action, FormEvent e ) throws Exception
  {
    action = action.trim();

    System.out.println( "action: " + action );
    Class cls = (Class)ApplicationHandler.handler.get( action );
    AbstractListener helper = (AbstractListener)cls.newInstance();
    helper.setNextPage( (String)handlerNext.get( action ) );
    helper.setDefaultNextPage( (String)handlerNext.get( action ) );
    helper.setAlternativeNextPage( (String)handlerANext.get( action ) );
    helper.handle( e );
  }

  /**
   * 
   * 
   * @param request
   */
  protected void showVersion( ServletRequest request )
  {
    request.setAttribute( "next", "snoopy.jsp" );
  }

  /**
   * 
   * 
   * @param configFile
   * 
   * @throws IOException
   * @throws SAXException
   */
  private static void initHandler( String configFile ) throws IOException, MalformedURLException,
      SAXException
  {
    /*
     * Read resource into Document...
     */
    URL url = new URL( configFile );
    Reader reader = new InputStreamReader( url.openStream() );
    Document doc = XMLTools.parse( reader );

    /*
     * Read and create page elements
     */
    NodeList nodes = doc.getElementsByTagName( EVENT );

    for( int i = 0; i < nodes.getLength(); i++ )
    {
      String name = XMLTools.getAttrValue( nodes.item( i ), NAME );
      String cls = XMLTools.getAttrValue( nodes.item( i ), CLASS );
      String nextPage = XMLTools.getAttrValue( nodes.item( i ), NEXT );
      String anextPage = XMLTools.getAttrValue( nodes.item( i ), ALTERNATIVENEXT );

      if( anextPage == null )
      {
        anextPage = nextPage;
      }

      Class clscls = null;

      try
      {
        clscls = Class.forName( cls );
        handler.put( name.trim(), clscls );
        handlerNext.put( name.trim(), nextPage );
        handlerANext.put( name.trim(), anextPage );
      }
      catch( Exception ex )
      {
        System.out.println( ex );
        throw new SAXException( "No handler class specified for event:" + name + " " + cls + "\n"
            + ex );
      }
    }
  }

  /** @link dependency */

  /* #FormEvent lnkFormEvent; */
}

/*
 * Changes to this class. What the people haven been up to:
 * 
 * $Log$
 * Revision 1.3  2004/10/07 14:09:19  doemming
 * *** empty log message ***
 *
 * Revision 1.1  2004/09/02 23:56:59  doemming
 * *** empty log message ***
 * Revision 1.3 2004/08/31 12:54:14 doemming
 * *** empty log message *** Revision 1.13 2004/04/27 06:40:43 poth no message
 * 
 * Revision 1.12 2004/02/27 15:56:58 mrsnyder Removed hard-coded catalog name.
 * 
 * Revision 1.11 2004/02/19 10:08:56 poth no message
 * 
 * Revision 1.10 2004/01/03 13:46:45 poth no message
 * 
 * Revision 1.9 2003/12/31 16:13:59 poth no message
 * 
 * Revision 1.8 2003/12/12 16:48:24 poth no message
 * 
 * Revision 1.7 2003/11/28 11:35:56 poth no message
 * 
 * Revision 1.6 2003/11/16 10:59:37 poth no message
 * 
 * Revision 1.5 2003/11/14 08:22:26 poth no message
 * 
 * Revision 1.4 2003/11/10 07:57:38 poth no message
 * 
 * Revision 1.3 2003/10/31 16:11:45 poth no message
 * 
 * Revision 1.2 2003/10/30 16:59:04 poth no message
 * 
 * Revision 1.1 2003/07/11 12:47:10 poth no message
 * 
 * Revision 1.1.1.1 2002/03/22 15:23:10 ap no message
 *  
 */
