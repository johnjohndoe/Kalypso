package org.kalypso.util.loader;

import java.util.Hashtable;
import java.util.Map;
import java.util.Properties;

import com.bce.util.ClassUtilities;
import com.bce.util.ClassUtilities.ClassUtilityException;

/**
 * @author schlienger
 *  
 */
public class DefaultLoaderFactory implements ILoaderFactory
{
  private final static Map m_loaders = new Hashtable();

  private Properties m_props;

  public DefaultLoaderFactory( Properties props )
  {
    m_props = props;
  }

  public ILoader getLoaderInstance( String type ) throws LoaderException
  {
    String className = m_props.getProperty( type );

    // z.Zeit unsere ShapeLoader
    ILoader loader = (ILoader)m_loaders.get( type );

    if( loader == null )
    {
      try
      {
        loader = (ILoader)ClassUtilities.newInstance( className, ILoader.class );
      }
      catch( ClassUtilityException e )
      {
        throw new LoaderException( e );
      }

      m_loaders.put( type, loader );
    }

    return loader;
  }
}