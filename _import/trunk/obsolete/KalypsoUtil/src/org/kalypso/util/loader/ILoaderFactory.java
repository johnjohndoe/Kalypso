package org.kalypso.util.loader;

import org.kalypso.util.factory.FactoryException;

/**
 * @author schlienger
 *  
 */
public interface ILoaderFactory
{
  public ILoader getLoaderInstance( final String type, final ClassLoader cl ) throws FactoryException;

  public String[] getTypes();
  
}