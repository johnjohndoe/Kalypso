/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

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

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.core.client;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import javax.xml.rpc.ServiceException;
import javax.xml.rpc.Stub;

/**
 * Factory for Kalypso WebService proxies.
 * 
 * @author schlienger
 */
public class ProxyFactory
{
  /** property containing the base classpath of all kalypso service proxies */
  public final static String KALYPSO_PROXY_BASE = "KALYPSO_PROXY_BASE";

  /** used to represent no arguments when invoking a method using reflection */
  protected final static Object[] NO_ARGS = new Object[0];

  /** used to represent no types arguments when getting a method using reflection */
  protected final static Class[] NO_TYPES = new Class[0];

  /** contains the proxies that were already created */
  private final Map m_proxies;

  /** used for instanciating the proxy implementations provided through extensions */
  private final ServiceProxies m_proxyImpls;

  /**
   * configuration of the factory
   * <p>
   * It needs following information:
   * <p>
   * KALYPSO_PROXY_BASE = the base classpath (package name or namespace) where the proxy classes are created.
   * <p>
   * <service_name>_URL = for each service that this factory should create. It contains the location of the servers that
   * can deliver the desired service.
   */
  private final Properties m_conf;

  /**
   * Configures this factory.
   * 
   * @param conf
   */
  public ProxyFactory( final Properties conf )
  {
    m_conf = conf;
    m_proxies = new HashMap();
    m_proxyImpls = new ServiceProxies();
  }

  /**
   * Returns the first available proxy of the Kalypso WebService that fullfills the desired interface
   * 
   * @param serviceName
   *          Name of the Service residing on the server
   * @param intfName
   *          Name of the interface that the service implements
   * @return stub
   * 
   * @throws ServiceException
   *           if stub or server unavailable
   */
  public Stub getAnyProxy( final String serviceName, final String intfName ) throws ServiceException
  {
    final List list = new ArrayList();

    for( final Iterator itServer = getServers( serviceName ).iterator(); itServer.hasNext(); )
    {
      final String serverUrl = (String)itServer.next();
      final Stub proxy = getCheckedProxy( serverUrl, serviceName, intfName, list );
      if( proxy != null )
        return proxy;
    }

    final Exception e = (Exception)( list.size() > 0 ? list.get( list.size() - 1 ) : new IllegalStateException(
        "Keine Server-URL gefunden. Siehe Kalypso-Konfiguration." ) );
    throw new ServiceException( "Dienst <" + serviceName + "> steht nicht zur Verfügung. Grund: " + e.getLocalizedMessage(), e );
  }

  /**
   * Returns the all available proxies of the Kalypso WebService that fullfill the desired interface.
   * 
   * @param serviceName
   *          Name of the Service residing on the server
   * @param intfName
   *          Name of the interface that the service implements
   * @return stub
   * 
   * @throws ServiceException
   *           if stub or server unavailable
   */
  public Stub[] getAllProxies( final String serviceName, final String intfName ) throws ServiceException
  {
    final Collection stubs = getAllProxiesAsMap( serviceName, intfName ).values();
    return (Stub[])stubs.toArray( new Stub[stubs.size()] );
  }

  /** Create a map key -> proxy-stub. The key is unique per proxy, but cannot be assumed to be anythin g meaningfull */
  public Map getAllProxiesAsMap( final String serviceName, final String intfName ) throws ServiceException
  {
    final List servers = getServers( serviceName );
    final Map stubs = new HashMap();
    for( final Iterator itServer = servers.iterator(); itServer.hasNext(); )
    {
      final String serverUrl = (String)itServer.next();
      final Stub proxy = getCheckedProxy( serverUrl, serviceName, intfName, null );
      if( proxy != null )
      {
        try
        {
          // create a service-unique key
          final URL url = new URL( serverUrl );
          stubs.put( url.getHost() + "-" + url.getPort(), proxy );
        }
        catch( final MalformedURLException e )
        {
          e.printStackTrace();
          // never happens, if proxy != null
        }
      }
    }

    return stubs;
  }

  /** Returns the URL of the servers as list of strings */
  private List getServers( final String serviceName ) throws ServiceException
  {
    /**
     * contains the list of Kalypso-Servers that are configured. When asking for a proxy, this method will go through
     * the list until it finds one available server that can deliver the corresponding service.
     */
    final String strServers = m_conf.getProperty( serviceName + "_URL" );
    if( strServers == null )
      throw new ServiceException( "Keine Serverkonfiguration gefunden. Stellen Sie sicher dass die Einstellungen\n"
          + " richtig sind, und dass mindestens ein Server zur Verfügung steht." );

    final List servers = Arrays.asList( strServers.split( "," ) );
    if( servers.size() == 0 )
      throw new ServiceException( "No Server found in configuration. Cannot proceed." );
    return servers;
  }

  /**
   * Gets the proxy and checks, if it is available.
   * 
   * @param exceptions
   *          if not null, the exceptions that might occur during the execution of this method are stored in the list
   * 
   * @return null, if proxy is not available
   */
  private Stub getCheckedProxy( final String serverUrl, final String serviceName, final String intfName,
      final List exceptions )
  {
    try
    {
      final Stub proxy = getProxy( serverUrl, serviceName, intfName );
      final Method m = proxy.getClass().getMethod( "getServiceVersion", NO_TYPES );
      m.invoke( proxy, NO_ARGS );

      // if we get here, service is ok
      return proxy;
    }
    catch( final InvocationTargetException e )
    {
      e.printStackTrace();
      
      if( exceptions != null )
        exceptions.add( e.getTargetException() );
    }
    catch( final Exception e ) // generic Exception is caught here for simplicity
    {
      e.printStackTrace();
      
      if( exceptions != null )
        exceptions.add( e );
    }
    
    return null;
  }

  /** Creates the proxy for the given endpoint. Will be cached. */
  private Stub getProxy( final String serverUrl, final String serviceName, final String intfName )
      throws ServiceException
  {
    String strEndPoint = serverUrl;
    
    if( !serverUrl.endsWith( "/" ) )
        strEndPoint += "/";

    strEndPoint += serviceName + "/" + intfName;
    
    if( m_proxies.containsKey( strEndPoint ) )
      return (Stub)m_proxies.get( strEndPoint );

    // TRICKY: we set the classloader because of a problem using jaxrpc at
    // runtime under Eclipse. It seems that the system class loader is explicitely
    // used by the jaxrpc jars and that's bad because it doesn't find the plugins
    // runtime libs.
    if( Thread.currentThread().getContextClassLoader() != getClass().getClassLoader() )
      Thread.currentThread().setContextClassLoader( getClass().getClassLoader() );

    try
    {
      final Object proxyImpl = m_proxyImpls.getInstanceFor( intfName );

      final Method method = proxyImpl.getClass().getMethod( "get" + intfName + "Port", NO_TYPES );

      final Stub proxy = (Stub)method.invoke( proxyImpl, NO_ARGS );

      // configure proxy with first server from configuration
      proxy._setProperty( javax.xml.rpc.Stub.ENDPOINT_ADDRESS_PROPERTY, strEndPoint );
      m_proxies.put( strEndPoint, proxy );
      return proxy;
    }
    catch( Exception e ) // generic exception caught for simplicity
    {
      throw new ServiceException( "Service " + strEndPoint + " not available. Could not create stub.", e );
    }
  }
}