package org.kalypso.loader;

import java.util.Properties;

import org.kalypso.util.factory.ConfigurableCachableObjectFactory;
import org.kalypso.util.factory.FactoryException;

/**
 * Die Standardimplementation der {@link ILoaderFactory}
 *
 * @author Schlienger
 *
 */
public class DefaultLoaderFactory extends ConfigurableCachableObjectFactory implements ILoaderFactory
{
  public DefaultLoaderFactory( final Properties props, final ClassLoader cl )
  {
    super( props, false, cl );
  }

  public ILoader getLoaderInstance( final String type ) throws FactoryException
  {
    return (ILoader)getObjectInstance( type, ILoader.class );
  }
}