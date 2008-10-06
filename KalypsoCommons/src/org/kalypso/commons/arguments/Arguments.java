package org.kalypso.commons.arguments;

import java.util.LinkedHashMap;

/**
 * Arguments is a simple extension of Properties with a nesting-capability.
 * 
 * @author schlienger
 */
public class Arguments extends LinkedHashMap<String, Object>
{
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

  /**
   * @return the nested-arguments from the given key
   */
  public Arguments getArguments( final String key )
  {
    return (Arguments)get( key );
  }
}
