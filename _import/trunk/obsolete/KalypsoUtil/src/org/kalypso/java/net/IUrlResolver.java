package org.kalypso.java.net;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Iterator;

/**
 * @author belger
 */
public interface IUrlResolver
{
  public URL resolveURL( final URL base, final String relative ) throws MalformedURLException;

  /** An iterator over entries of a map (Map.Entry)
   * Each entry represant a token , wich can be replaced
   * */
  public Iterator getReplaceEntries();

  /** add a Replace token to the map, which can be accessed via getReplaceEntries() */
  public void addReplaceToken( final String key, final String value );
}
