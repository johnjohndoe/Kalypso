package org.kalypso.java.net;

import java.io.IOException;
import java.net.URL;
import java.net.URLConnection;
import java.net.URLStreamHandler;
import java.util.Hashtable;
import java.util.Map;

/**
 * A URL context replacement based <code>URLStreamHandler</code>.
 * <p>
 * Removes the protocol of the <code>URL</code> and builds a new URL
 * using the mapped context URL for this protocol.
 * <p>
 * Example:
 * <pre>
 * URL input: project:foo/bar/test.txt#script#foobar
 * 
 * protocol: project
 * replacement: URL=file:C:/Temp/Project/
 * 
 * the URL will be created using the following syntax:
 * URL=file:C:/Temp/Project/foo/bar/test.txt#script#foobar
 * </pre>
 * 
 * @author schlienger
 */
public class ContextMappingURLStreamHandler extends URLStreamHandler
{
  private final Map m_replacements = new Hashtable();

  /**
   * Sets the replacement for the given protocol string.
   */
  public void setStringReplacement( final String protocol, final URL context )
  {
    m_replacements.put( protocol, context );
  }

  /**
   * @see java.net.URLStreamHandler#openConnection(java.net.URL)
   */
  protected URLConnection openConnection( final URL u ) throws IOException
  {
    final String protocol = u.getProtocol();

    if( !m_replacements.containsKey( protocol ) )
      return u.openConnection();

    final URL context = (URL)m_replacements.get( protocol );

    return new URL( context, u.toExternalForm().replaceFirst( protocol + ":", "" ) ).openConnection();
  }
}