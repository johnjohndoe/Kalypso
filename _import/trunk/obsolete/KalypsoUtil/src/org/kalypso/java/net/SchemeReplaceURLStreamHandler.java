package org.kalypso.java.net;

import java.io.IOException;
import java.net.URL;
import java.net.URLConnection;
import java.net.URLStreamHandler;
import java.util.Properties;

/**
 * Replaces the scheme of the URL with the corresponding string. Builds up a new
 * URL with 'file' as protocol and the replaced string as the rest of the spec.
 * 
 * @author schlienger
 */
public class SchemeReplaceURLStreamHandler extends URLStreamHandler
{
  final private Properties m_replaceProps = new Properties();

  /**
   * 
   */
  public void setSchemeReplacement( final String scheme, final String replacement )
  {
    m_replaceProps.setProperty( scheme, replacement );
  }
  
  /**
   * @see java.net.URLStreamHandler#openConnection(java.net.URL)
   */
  protected URLConnection openConnection( final URL u ) throws IOException
  {
    final String protocol = u.getProtocol();
    
    if( !m_replaceProps.containsKey( protocol ) )
      return u.openConnection();
    
    final String replacement = m_replaceProps.getProperty( protocol, "" );
    
    String urlString = u.toExternalForm();
    urlString = urlString.replaceFirst( protocol + ":", "" );
    urlString = "file:" + replacement + urlString;
    
    return new URL( urlString ).openConnection();
  }
}