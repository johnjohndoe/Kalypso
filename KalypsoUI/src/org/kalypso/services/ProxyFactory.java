package org.kalypso.services;

import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import javax.xml.rpc.ServiceException;
import javax.xml.rpc.Stub;

import org.kalypso.java.lang.reflect.ClassUtilities;

/**
 * 
 * 
 * @author schlienger
 */
public class ProxyFactory
{
  /** property containing the base classpath of all kalypso service proxies */
  public final static String KALYPSO_PROXY_BASE = "KALYPSO_PROXY_BASE";

  /** used to represent no arguments when invoking a method using reflection */
  private final static Object[] NO_ARGS = new Object[0];

  /** used to represent no types arguments when getting a method using reflection */
  private final static Class[] NO_TYPES = new Class[0];

  /** contains the proxies that were already created */
  private final Map m_proxies;

  /**
   * configuration of the factory
   * <p>
   * It needs following information:
   * <p>
   * KALYPSO_PROXY_BASE = the base classpath (package name or namespace) where
   * the proxy classes are created.
   * <p>
   * <service_name>_URL = for each service that this factory should create. It
   * contains the location of the servers that can deliver the desired service. 
   */
  private Properties m_conf;

  /**
   * Configures this factory.
   */
  public ProxyFactory( final Properties conf )
  {
    m_conf = conf;
    m_proxies = new HashMap();
  }

  /**
   * Returns the proxy of the Kalypso WebService that fullfills the desired
   * interface
   * 
   * @param serviceName
   *          Name of the Service residing on the server
   * @param intfName
   *          Name of the interface that the service implements
   * 
   * @throws ServiceException
   *           if stub or server unavailable
   */
  public Stub getProxy( final String serviceName, final String intfName ) throws ServiceException
  { 
    /** contains the list of Kalypso-Servers that are configured. When asking
     * for a proxy, this method will go through the list until it finds one
     * available server that can deliver the corresponding service.
     */
    final List m_servers = Arrays.asList( m_conf.getProperty( serviceName + "_URL" ).split(";") );

    // used as key in the proxies map
    final String key = serviceName + "-" + intfName;

    Stub proxy = null;

    final Iterator itServer = m_servers.iterator();
    if( !itServer.hasNext() )
      throw new ServiceException( "No Server found in configuration. Cannot proceed." );

    if( !m_proxies.containsKey( key ) )
    {
      final String strProxyClass = m_conf.getProperty( KALYPSO_PROXY_BASE ) + "." + serviceName + "_Impl";

      try
      {
        Class c = Class.forName("javax.activation.DataSource");
        System.out.println( c );
        
        final Object proxyImpl = ClassUtilities.newInstance( strProxyClass, Object.class, getClass().getClassLoader() );

        final Method method = proxyImpl.getClass().getMethod( "get" + intfName + "Port", NO_TYPES );

        proxy = (Stub)method.invoke( proxyImpl, NO_ARGS );

        // configure proxy with first server from configuration
        final String serverUrl = (String)itServer.next();
        final String strEndPoint = serverUrl + "/" + serviceName + "/" + intfName;
        proxy._setProperty( javax.xml.rpc.Stub.ENDPOINT_ADDRESS_PROPERTY, strEndPoint );
      }
      catch( Exception e ) // generic Exception caught for simplicity
      {
        throw new ServiceException( "Service " + key + " not available. Could not create stub.", e );
      }

      m_proxies.put( key, proxy );
    }
    else
      proxy = (Stub)m_proxies.get( key );

    boolean done = false;
    while( !done )
    {
      try
      {
        // check availability of the service (note: it is a prerequesite that
        // each Kalypso Service has a getServiceVersion() method in order to
        // check the presence of the WebService and its version number)
        Method m = proxy.getClass().getMethod( "getServiceVersion", NO_TYPES );

        m.invoke( proxy, NO_ARGS );

        done = true;
      }
      catch( Exception e ) // generic Exception is caught here for simplicity
      {
        if( itServer.hasNext() )
        {
          // reconfigure proxy with another server
          final String serverUrl = (String)itServer.next();
          final String strEndPoint = serverUrl + "/" + serviceName + "/" + intfName;
          proxy._setProperty( javax.xml.rpc.Stub.ENDPOINT_ADDRESS_PROPERTY, strEndPoint );
        }
        else
          throw new ServiceException( "Service " + key
              + " not available. Could not find an servicing server." );
      }
    }

    return proxy;
  }
}