package org.kalypso.util.loader;


/**
 * @author schlienger
 *
 */
public interface ILoaderFactory
{
  public ILoader getLoaderInstance( String type ) throws LoaderException;
}
