/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.commons.net;

import java.net.URL;
import java.util.List;

import org.apache.commons.httpclient.Credentials;
import org.apache.commons.httpclient.DefaultHttpMethodRetryHandler;
import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.UsernamePasswordCredentials;
import org.apache.commons.httpclient.auth.AuthScope;
import org.apache.commons.httpclient.params.HttpMethodParams;
import org.kalypso.contribs.eclipse.core.net.Proxy;

/**
 * This class helps with the proxy.
 * 
 * @author Holger Albert
 */
public class ProxyUtilities
{
  /**
   * The constructor.
   */
  private ProxyUtilities( )
  {
  }

  /**
   * This function returns an instance of the proxy class for retrieving the proxy data.
   * 
   * @return The proxy object.
   */
  public static Proxy getProxy( )
  {
    return new Proxy();
  }

  /**
   * Creates a configured http client. The configuration includes setting of proxy settings.
   * <p>
   * IMPORTANT: To use proxy-authentication, you must use the setDoAuthentication Method of the HttpMethod you are going
   * to use.
   * </p>
   * <strong>Example:</strong>
   * 
   * <pre>
   * HttpMethod method = new GetMethod( m_url.toString() );
   * method.setDoAuthentication( true );
   * </pre>
   * 
   * @param timeout
   *            The socket timeout in milliseconds.
   * @param retries
   *            The number of retries, the http client is going to make. Set to a value lower then 0 to leave it at the
   *            default value.
   * @return The configured http client. If no proxy is set, it will be a normal http client with the given timeout and
   *         retries.
   * @deprecated This method should not be used any more, because its functionality is covered completely by the method
   *             {@link #getConfiguredHttpClient(int, URL, int)}. If you have no appropriate URL, leave it null.
   */
  @Deprecated
  public static HttpClient getConfiguredHttpClient( int timeout, int retries )
  {
    /* Create the new http client. */
    HttpClient client = new HttpClient();

    /* Client should always authenticate before making a connection. */
    client.getParams().setAuthenticationPreemptive( true );
    client.getParams().setSoTimeout( timeout );

    /* If a retry count is given, set the number of retries. */
    if( retries >= 0 )
      client.getParams().setParameter( HttpMethodParams.RETRY_HANDLER, new DefaultHttpMethodRetryHandler( retries, true ) );

    /* Get the proxy object. */
    Proxy proxy = getProxy();

    /* If the proxy should be used, configure the client with it. */
    if( proxy.useProxy() )
    {
      /* Get the proxy data. */
      String proxyHost = proxy.getProxyHost();
      int proxyPort = proxy.getProxyPort();

      /* Set the proxy information. */
      client.getHostConfiguration().setProxy( proxyHost, proxyPort );

      /* Get the credentials. */
      String user = proxy.getUser();
      String password = proxy.getPassword();

      /* Set them, if the credentials are complete. */
      if( user != null && password != null )
      {
        Credentials credentials = new UsernamePasswordCredentials( user, password );
        client.getState().setProxyCredentials( AuthScope.ANY, credentials );
      }
    }

    return client;
  }

  /**
   * Creates a configured http client. The configuration includes setting of proxy settings.
   * <p>
   * IMPORTANT: To use proxy-authentication, you must use the setDoAuthentication Method of the HttpMethod you are going
   * to use.
   * </p>
   * <strong>Example:</strong>
   * 
   * <pre>
   * HttpMethod method = new GetMethod( m_url.toString() );
   * method.setDoAuthentication( true );
   * </pre>
   * 
   * @param timeout
   *            The socket timeout in milliseconds.
   * @param url
   *            The url, for which the client is needed. Could be null.
   * @param retries
   *            The number of retries, the http client is going to make. Set to a value lower then 0 to leave it at the
   *            default value.
   * @return The configured http client. If no proxy is set or the host, included in the url is a non proxy host, it
   *         will be a normal http client with the given timeout and retries. If url is null, the check for non proxy
   *         hosts is omitted.
   */
  public static HttpClient getConfiguredHttpClient( int timeout, URL url, int retries )
  {
    /* Create the new http client. */
    HttpClient client = new HttpClient();

    /* Client should always authenticate before making a connection. */
    client.getParams().setAuthenticationPreemptive( true );
    client.getParams().setSoTimeout( timeout );

    /* If a retry count is given, set the number of retries. */
    if( retries >= 0 )
      client.getParams().setParameter( HttpMethodParams.RETRY_HANDLER, new DefaultHttpMethodRetryHandler( retries, true ) );

    /* Get the proxy object. */
    Proxy proxy = getProxy();

    /* If the proxy should be used, configure the client with it. */
    if( proxy.useProxy() && !isNonProxyHost( url ) )
    {
      /* Get the proxy data. */
      String proxyHost = proxy.getProxyHost();
      int proxyPort = proxy.getProxyPort();

      /* Set the proxy information. */
      client.getHostConfiguration().setProxy( proxyHost, proxyPort );

      /* Get the credentials. */
      String user = proxy.getUser();
      String password = proxy.getPassword();

      /* Set them, if the credentials are complete. */
      if( user != null && password != null )
      {
        Credentials credentials = new UsernamePasswordCredentials( user, password );
        client.getState().setProxyCredentials( AuthScope.ANY, credentials );
      }
    }

    return client;
  }

  /**
   * This function schecks if the host of an url is one of the non proxy hosts.
   * 
   * @param url
   *            The url to check.
   * @return True, if the host, contained in the url should not use a proxy.
   */
  public static boolean isNonProxyHost( URL url )
  {
    /* Without a URL, proxy settings should be applied normally. */
    if( url == null )
      return false;

    /* Get the proxy object. */
    Proxy proxy = getProxy();

    /* All hosts, that should use no proxy. */
    List<String> nonProxyHosts = proxy.getNonProxyHosts();

    for( int i = 0; i < nonProxyHosts.size(); i++ )
    {
      if( url.getHost().equals( nonProxyHosts.get( i ) ) )
        return true;
    }

    return false;
  }
}