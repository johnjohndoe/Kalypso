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
public class ConfigurableCachableObjectFactory
{
  private final Properties m_props;
  private final Map m_objects = new Hashtable();
  private final boolean m_cache;

  /**
   * @param props Die Keys sind die in getObjectInstance benutzten type's, die Values sind die
   * namen der Klassen, die jeweils erzeugt werden
   * @param cache falls true, werden die erzeugten Objekte gecached, sonst wird immer ein neues Objekt erzeugt
   */
  public ConfigurableCachableObjectFactory( final Properties props, final boolean cache )
  {
    m_props = props;   
    m_cache = cache;
  }
  
  public Object getObjectInstance( final String type, final Class expected, final ClassLoader cl ) throws FactoryException
  {
    String className = m_props.getProperty( type );

    Object obj = null;
    
    if( m_cache )
      obj = m_objects.get( type );

    if( obj == null )
    {
      try
      {
        obj = ClassUtilities.newInstance( className, expected, cl );
      }
      catch( ClassUtilityException e )
      {
        throw new FactoryException( e );
      }

      if( m_cache )
        m_objects.put( type, obj );
    }

    return obj;
  }
  
  protected final Properties getProperties()
  {
    return m_props;
  }
}
