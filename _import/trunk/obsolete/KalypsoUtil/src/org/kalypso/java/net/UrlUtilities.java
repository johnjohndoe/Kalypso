package org.kalypso.java.net;

import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;

/**
 * @author belger
 */
public class UrlUtilities implements IUrlResolver
{
  public UrlUtilities()
  {
  // do not instantiate
  }

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
}