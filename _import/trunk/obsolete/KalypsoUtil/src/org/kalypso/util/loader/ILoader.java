package org.kalypso.util.loader;

/**
 * @author schlienger
 *
 */
public interface ILoader
{
  public Object load( String source ) throws LoaderException;
}
