package org.kalypso.util.factory;

import java.util.Hashtable;
import java.util.Map;
import java.util.Properties;

import org.kalypso.util.reflect.ClassUtilities;
import org.kalypso.util.reflect.ClassUtilities.ClassUtilityException;

/**
 * Eine sehr algemeine Factory von Objekte die durch Properties konfiguriert wird.
 * 
 * @author schlienger
 */
public class ConfigurableCachedObjectFactory
{
  private final Properties m_props;
  private final Map m_objects = new Hashtable();

  public ConfigurableCachedObjectFactory( final Properties props )
  {
    m_props = props;   
  }
  
  public Object getObjectInstance( String type, Class expected ) throws FactoryException
  {
    String className = m_props.getProperty( type );

    Object obj = m_objects.get( type );

    if( obj == null )
    {
      try
      {
        obj = ClassUtilities.newInstance( className, expected );
      }
      catch( ClassUtilityException e )
      {
        throw new FactoryException( e );
      }

      m_objects.put( type, obj );
    }

    return obj;
  }
}
