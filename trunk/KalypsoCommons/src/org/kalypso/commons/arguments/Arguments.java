package org.kalypso.commons.arguments;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * Arguments
 * 
 * @author schlienger
 */
public class Arguments implements Map
{
  private Map m_map = new HashMap();

  public String getProperty( final String key )
  {
    final Object object = get( key );
    if( object == null )
      return null;

    return object.toString();
  }

  public String getProperty( final String key, final String defaultValue )
  {
    final Object object = get( key );
    if( object == null )
      return defaultValue;

    return object.toString();
  }

  public void clear()
  {
    m_map.clear();
  }

  public boolean containsKey( Object key )
  {
    return m_map.containsKey( key );
  }

  public boolean containsValue( Object value )
  {
    return m_map.containsValue( value );
  }

  public Set entrySet()
  {
    return m_map.entrySet();
  }

  public boolean equals( Object obj )
  {
    return m_map.equals( obj );
  }

  public Object get( Object key )
  {
    return m_map.get( key );
  }

  public int hashCode()
  {
    return m_map.hashCode();
  }

  public boolean isEmpty()
  {
    return m_map.isEmpty();
  }

  public Set keySet()
  {
    return m_map.keySet();
  }

  public Object put( Object key, Object value )
  {
    return m_map.put( key, value );
  }

  public void putAll( Map t )
  {
    m_map.putAll( t );
  }

  public Object remove( Object key )
  {
    return m_map.remove( key );
  }

  public int size()
  {
    return m_map.size();
  }

  public String toString()
  {
    return m_map.toString();
  }

  public Collection values()
  {
    return m_map.values();
  }

  public Arguments getArguments( final String key )
  {
    return (Arguments)get( key );
  }
}
