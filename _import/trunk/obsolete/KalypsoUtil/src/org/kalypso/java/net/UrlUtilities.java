package org.kalypso.java.net;

import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.Iterator;
import java.util.Properties;

/**
 * @author belger
 */
public class UrlUtilities implements IUrlResolver
{
  private Properties m_replaceTokenMap = new Properties();
  
  /**
   * <p>Resolves a (potential) relative URL to a base URL.</p>
   * 
   * @param baseURL
   *          URL, to which the relative url will be resolved
   * @param relativeURL
   *          a string designating an absolute or relative URL
   * @return if relativeURL is relative, return new URL( baseURL, relativeURL ),
   *         else return new URL( relativeURL )
   * @throws MalformedURLException
   */
  public URL resolveURL( final URL baseURL, final String relativeURL )
      throws MalformedURLException
  {
    try
    {
      final URI uri = new URI( relativeURL );
      return uri.isAbsolute() ? new URL( relativeURL ) : new URL( baseURL, relativeURL );
    }
    catch( final URISyntaxException e )
    {
      throw new MalformedURLException( e.getLocalizedMessage() );
    }
  }
  
  /**
   * @see org.kalypso.java.net.IUrlResolver#getReplaceEntries()
   */
  public final Iterator getReplaceEntries()
  {
    return m_replaceTokenMap.entrySet().iterator();
  }

  /**
   * @see org.kalypso.java.net.IUrlResolver#addReplaceToken(java.lang.String, java.lang.String)
   */
  public void addReplaceToken( final String key, final String value )
  {
    m_replaceTokenMap.setProperty( key, value );
  }
}