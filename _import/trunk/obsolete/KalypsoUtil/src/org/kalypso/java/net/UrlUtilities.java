package org.kalypso.java.net;

import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;

/**
 * @author belger
 */
public class UrlUtilities
{
  private UrlUtilities()
  {
  // do not instantiate
  }

  /**
   * Resolves a (potential) relative URL to a base URL.
   * 
   * @param baseURL
   *          URL, to which the relative url will be resolved
   * @param relativeURL
   *          a string designating an absolute or relative URL
   * @return if relativeURL is relative, return new URL( baseURL, relativeURL ),
   *         else return new URL( relativeURL )
   * @throws MalformedURLException
   */
  public static URL resolveURL( final URL baseURL, final String relativeURL )
      throws MalformedURLException
  {
    try
    {
      URI uri = new URI( relativeURL );
      if( !uri.isAbsolute() )
      {
        String prefix = baseURL.toString();
        final int slashIndex = prefix.lastIndexOf( "/" );
        final int backslashIndex = prefix.lastIndexOf( "\\" );
        final int index = Math.max( slashIndex, backslashIndex );

        prefix = prefix.substring( 0, index + 1 );

        uri = new URI( prefix + relativeURL );
      }
      return new URL( baseURL, uri.getPath() );
    }
    catch( final URISyntaxException e )
    {
      throw new MalformedURLException( e.getLocalizedMessage() );
    }
  }
}