package org.kalypso.util.loader;

import java.util.Properties;

import org.kalypso.util.factory.ConfigurableCachedObjectFactory;
import org.kalypso.util.factory.FactoryException;

/**
 * @author schlienger
 *  
 */
public class DefaultLoaderFactory extends ConfigurableCachedObjectFactory implements ILoaderFactory
{
  public DefaultLoaderFactory( Properties props )
  {
    super( props );
  }

  public ILoader getLoaderInstance( String type ) throws FactoryException
  {
    return (ILoader)getObjectInstance( type, ILoader.class );
  }
}

  /**
   * @see org.kalypso.util.loader.ILoaderFactory#getLoaderControl(java.lang.String)
   */
  public ILoaderUI getLoaderControl( final String type ) throws LoaderException
  {
    final String className = m_props.getProperty( type + "_ui" );
    
    try
    {
      return (ILoaderUI)ClassUtilities.newInstance( className, ILoaderUI.class );
    }
    catch( ClassUtilityException e )
    {
      throw new LoaderException( e );
    }
  }
}