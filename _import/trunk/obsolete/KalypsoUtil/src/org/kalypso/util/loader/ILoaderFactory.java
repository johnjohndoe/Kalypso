package org.kalypso.util.loader;


/**
 * @author schlienger
 *
 */
public interface ILoaderFactory
{
  public ILoader getLoaderInstance( final String type ) throws LoaderException;
  
  public ILoaderUI getLoaderControl( final String type ) throws LoaderException;
}
