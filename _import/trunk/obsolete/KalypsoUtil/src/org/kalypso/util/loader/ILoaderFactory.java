package org.kalypso.util.loader;

import org.kalypso.util.factory.FactoryException;


/**
 * @author schlienger
 *
 */
public interface ILoaderFactory
{
  public ILoader getLoaderInstance( String type ) throws FactoryException;
  
    public ILoaderUI getLoaderControl( final String type ) throws LoaderException;
}
