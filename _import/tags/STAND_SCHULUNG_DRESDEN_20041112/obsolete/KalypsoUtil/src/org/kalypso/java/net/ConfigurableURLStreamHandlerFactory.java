package org.kalypso.java.net;

import java.net.URLStreamHandler;
import java.net.URLStreamHandlerFactory;
import java.util.HashMap;
import java.util.Map;

/**
 * A configurable <code>URLStreamHandlerFactory</code>. You can set various
 * <code>URLStreamHandler</code> that match specific protocols. 
 * 
 * @author schlienger
 */
public class ConfigurableURLStreamHandlerFactory implements URLStreamHandlerFactory
{
  final Map m_map = new HashMap();

  public void setHandler( final String protocol, final URLStreamHandler handler )
  {
    m_map.put( protocol, handler );
  }

  /**
   * @see java.net.URLStreamHandlerFactory#createURLStreamHandler(java.lang.String)
   */
  public URLStreamHandler createURLStreamHandler( final String protocol )
  {
    if( !m_map.containsKey( protocol ) )
      return null; // let default Java implementation decide what to do...

    final URLStreamHandler handler = (URLStreamHandler)m_map.get( protocol );
    return handler;
  }
}