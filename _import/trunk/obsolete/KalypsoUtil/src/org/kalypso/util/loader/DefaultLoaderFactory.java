package org.kalypso.util.loader;

import java.util.Properties;

import org.kalypso.util.factory.ConfigurableCachableObjectFactory;
import org.kalypso.util.factory.FactoryException;

/**
 * @author schlienger
 *  
 */
public class DefaultLoaderFactory extends ConfigurableCachableObjectFactory implements ILoaderFactory
{
  public DefaultLoaderFactory( final Properties props )
  {
    super( props, false );
  }

  public ILoader getLoaderInstance( final String type, final ClassLoader cl ) throws FactoryException
  {
    return (ILoader)getObjectInstance( type, ILoader.class, cl );
  }

  /**
   * @see org.kalypso.util.loader.ILoaderFactory#getTypes()
   */
  public String[] getTypes()
  {
    return (String[])getProperties().keySet().toArray( new String[] {} );
  }
}