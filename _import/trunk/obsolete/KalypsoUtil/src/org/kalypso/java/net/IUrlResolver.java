package org.kalypso.java.net;

import java.net.MalformedURLException;
import java.net.URL;

/**
 * @author belger
 */
public interface IUrlResolver
{
  public URL resolveURL( final URL base, final String relative ) throws MalformedURLException;
}
