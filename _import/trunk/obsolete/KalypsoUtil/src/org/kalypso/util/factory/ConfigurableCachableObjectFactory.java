package org.kalypso.util.factory;

import java.util.Hashtable;
import java.util.Map;
import java.util.Properties;

import org.kalypso.java.reflect.ClassUtilities;
import org.kalypso.java.reflect.ClassUtilities.ClassUtilityException;

/**
 * <p>
 * Eine sehr allgemeine Factory, die durch Properties konfiguriert wird.
 * </p>
 * <p>
 * Die Values der Properties bezeichnen den Klassennamen der zu erzeugenden
 * Objekte, die Keys dürfen beliebiege String sein.
 * </p>
 * <p>
 * Die Factory kann so konfiguriert werden, dass jede Objektart nur einmal
 * erzeugt wird, einmal erzeugte Objekte werden dann gecached und bei
 * nochmaliger Anfrage erneut zurückgegeben.
 * </p>
 * <p>
 * Beim Erzeugen eines Objects kann zusätzlich angegeben werden, ob dieses von
 * einer bestimmten Klasse ableiten soll. Erfüllt das Object diese Forderung
 * nicht wird eine Exception geworfen.
 * </p>
 * <p>
 * Die Properties sind erweiterbar.
 * 
 * @author schlienger
 */
public class ConfigurableCachableObjectFactory
{
  private final Properties m_props = new Properties();

  private final Map m_objects = new Hashtable();

  private final boolean m_cache;

  private final ClassLoader m_classLoader;

  /**
   * @param props
   *          Die Keys sind die in getObjectInstance benutzten type's, die
   *          Values sind die namen der Klassen, die jeweils erzeugt werden
   * @param cache
   *          falls true, werden die erzeugten Objekte gecached, sonst wird
   *          immer ein neues Objekt erzeugt
   */
  public ConfigurableCachableObjectFactory( final Properties props, final boolean cache,
      final ClassLoader cl )
  {
    m_props.putAll( props );
    m_cache = cache;
    m_classLoader = cl;
  }

  /**
   * @see ConfigurableCachableObjectFactory#getObjectInstance(String, Class,
   *      Object[])
   */
  public Object getObjectInstance( final String type, final Class expected )
      throws FactoryException
  {
    return getObjectInstance( type, expected, null );
  }

  /**
   * Erzeugt eine neue Instanz oder benutzt die bestehende Instanz aus dem
   * Cache, wenn aktiviert.
   * <p>
   * Dabei sollte man achten dass die Instanz auch über argumente instanziiert
   * werden kann. Wenn der Cache aktiviert ist, und man versucht andere
   * Argumente, aber die gleiche type von Object zu instanziieren, bekommt man
   * die Instanz aus dem Cache.
   * <p>
   * WICHTIG: Dies bedeutet dass man also Argumente benutzen sollte nur wenn der
   * Cache nicht aktiviert ist.
   */
  public Object getObjectInstance( final String type, final Class expected, final Object[] arguments )
      throws FactoryException
  {
    final String className = m_props.getProperty( type );
    if( className == null )
      throw new FactoryException( "Unknown type: " + type );

    Object obj = null;

    if( m_cache )
      obj = m_objects.get( type );

    if( obj == null )
    {
      try
      {
        obj = ClassUtilities.newInstance( className, expected, m_classLoader, arguments );
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

  /**
   * Inserts the given properties in the main properties of this factory.
   */
  public void addProperties( Properties props )
  {
    m_props.putAll( props );
  }
}