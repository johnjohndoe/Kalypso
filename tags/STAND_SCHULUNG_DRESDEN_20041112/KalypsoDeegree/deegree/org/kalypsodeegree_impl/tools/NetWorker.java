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
package org.deegree_impl.tools;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.net.Authenticator;
import java.net.PasswordAuthentication;
import java.net.URL;
import java.net.URLConnection;

import sun.misc.BASE64Encoder;

/**
 * Performs a HTTP request using the service URL submitted to the constructor
 * 
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 */
public class NetWorker
{
  private static final int GET = 0;

  private static final int POST = 1;

  private String contentType = null;

  private String request = null;

  private URL url = null;

  private int reqType = -1;

  private String encoding = null;

  /**
   * constructor for initializing a HTTP GET connection with UTF-8 as character
   * encoding
   * 
   * @param url
   *          URL to the net resource containing the URI
   */
  public NetWorker( URL url )
  {
    Debug.debugMethodBegin( this, "NetWorker(String strUrl)" );
    this.reqType = GET;
    this.url = url;
    this.encoding = "UTF-8";
    Debug.debugMethodEnd();
  }

  /**
   * constructor for initializing a HTTP GET connection with a user defined
   * encoding
   * 
   * @param encoding
   *          desired character encoding
   * @param url
   *          URL to the net resource containing the URI
   */
  public NetWorker( String encoding, URL url )
  {
    Debug.debugMethodBegin( this, "NetWorker(String strUrl)" );
    this.reqType = GET;
    this.url = url;
    this.encoding = encoding;
    Debug.debugMethodEnd();
  }

  /**
   * constructor for initializing a HTTP POST connection with UTF-8 as character
   * encoding
   * 
   * @param url
   *          URL to the net resource (without URI parameters)
   * @param request
   *          request that shall be posted to the net resource
   */
  public NetWorker( URL url, String request )
  {
    Debug.debugMethodBegin( this, "NetWorker(String strUrl)" );
    this.reqType = POST;
    this.url = url;
    this.request = request;
    this.encoding = "UTF-8";
    Debug.debugMethodEnd();
  }

  /**
   * constructor for initializing a HTTP POST connection with a user defined
   * encoding
   * 
   * @param encoding
   *          desired character encoding
   * @param url
   *          URL to the net resource (without URI parameters)
   * @param request
   *          request that shall be posted to the net resource
   */
  public NetWorker( String encoding, URL url, String request )
  {
    Debug.debugMethodBegin( this, "NetWorker(String strUrl)" );
    this.reqType = POST;
    this.url = url;
    this.request = request;
    this.encoding = encoding;
    Debug.debugMethodEnd();
  }

  /**
   * returns the content type of the response from the connected net resource.
   * this method shall be called after <tt>getInputStream</tt> or
   * <tt>getDataAsByteArr</tt> has been called.
   */
  public String getContentType()
  {
    return contentType;
  }

  /**
   * sends the request that have been passed to the constructor without
   * expecting to receive a response.
   */
  public void sendRequest() throws IOException
  {
    Debug.debugMethodBegin( this, "getInputStream" );

    // open connection to the requested host
    URLConnection connection = url.openConnection();

    connection.setDoInput( false );

    // sets the content type of the request
    connection.setRequestProperty( "Content-Type", "text/xml" );

    if( ( reqType == POST ) && ( request != null ) )
    {
      connection.setDoOutput( true );

      // get connection stream
      OutputStreamWriter osw = new OutputStreamWriter( connection.getOutputStream(), encoding );
      PrintWriter os = new PrintWriter( osw );

      // write post request into stream
      os.print( request );
      os.close();
    }
    else
    {
      connection.setDoOutput( false );
    }

    Debug.debugMethodEnd();
  }

  /**
   * returns an <tt>InputStream</tt> from the et resource
   * 
   * @return InputStream accessing the net resource
   * 
   * @throws IOException
   */
  public InputStream getInputStream() throws IOException
  {
    Debug.debugMethodBegin( this, "getInputStream" );

    // open connection to the requested host
    final URLConnection connection = url.openConnection();
    configureProxy( connection );
    connection.setDoInput( true );

    // sets the content type of the request
    connection.setRequestProperty( "Content-Type", "text/xml" );

    // provide result stream
    InputStream is = null;

    if( ( reqType == POST ) && ( request != null ) )
    {
      connection.setDoOutput( true );

      // get connection stream
      OutputStreamWriter osw = new OutputStreamWriter( connection.getOutputStream(), encoding );
      PrintWriter os = new PrintWriter( osw );

      // write post request into stream
      os.print( request );
      os.close();
    }
    else
    {
      connection.setDoOutput( false );
    }

    // reads the content type of the connected net resource
    try
    {
      contentType = connection.getHeaderField( "Content-Type" );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }

    // get result of the request
    try
    {
      is = connection.getInputStream();
    }
    catch( Exception e )
    {
      throw new IOException( "could not provide data: " + e );
    }

    Debug.debugMethodEnd();
    return is;
  }

  /**
   * Configures a URLConnection for Acces via proxy
   * 
   * @TODO: move to URLConnectionUtilities-Class
   * @TODO: handle https
   */
  public static void configureProxy( final URLConnection connection )
  {
    final URL url = connection.getURL();
    final PasswordAuthentication authentication = Authenticator.requestPasswordAuthentication( url.getHost(), null, url.getPort(), url.getProtocol(), "Blubberdibla", url.getAuthority() );
    
    if( authentication != null )
    {
      final String pw = authentication.getUserName() + ":" + authentication.getPassword();
      final String epw = "Basic " + ( new BASE64Encoder() ).encode( pw.getBytes() );

      connection.addRequestProperty( "Proxy-Authorization", epw );
    }
  }

  /**
   * performs the request and returns the result as a byte array.
   * 
   * @param expectedDataSize
   *          size a the data in bytes expected to be returned from the net
   *          resource. this value will be replaced if the resource is able to
   *          return the available data size.
   * @return a byte array containing the content of the net resource
   * 
   * @throws IOException
   */
  public byte[] getDataAsByteArr( int expectedDataSize ) throws IOException
  {
    Debug.debugMethodBegin( this, "getDataAsByteArr" );

    InputStream is = getInputStream();

    if( expectedDataSize <= 0 )
    {
      expectedDataSize = 10000;
    }

    ByteArrayOutputStream bos = new ByteArrayOutputStream( expectedDataSize );

    int v = 0;

    // write result to a ByteArrayOutputStream
    while( ( v = is.read() ) > -1 )
    {
      bos.write( v );
    }

    bos.flush();
    bos.close();

    is.close();

    Debug.debugMethodEnd();

    // return result as byte array
    return bos.toByteArray();
  }

  /**
   * Returns the original form of a <tt>URL</tt> as as <tt>String</tt>.
   * Handles local filenames correctly, C:/foo is formatted as file:///C:/foo
   * (and not as file:/C:/foo as returned by the toString () method of the <tt
   * <URL</tt> object.
   * <p>
   * 
   * @param url
   *          <tt>URL</tt> to be converted
   * @return <tt>String</tt> representation of the given <tt>URL</tt>
   */
  public static synchronized String url2String( URL url )
  {
    String port = "";

    if( url.getPort() != -1 )
    {
      port = ":" + url.getPort();
    }

    String s = url.getProtocol() + "://" + url.getHost() + port + url.getPath();//+"?"+query;

    return s;
  }

  /**
   * returns true if a connection to the submitted <tt>URL</tt> can be opend
   */
  public static synchronized boolean existsURL( URL url )
  {
    try
    {
      URLConnection con = url.openConnection();
      con.connect();
      con.getContentType();
    }
    catch( Exception e )
    {
      return false;
    }
    return true;
  }
}