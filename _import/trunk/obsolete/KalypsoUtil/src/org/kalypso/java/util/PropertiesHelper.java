package org.kalypso.java.util;

import java.util.Iterator;
import java.util.Map;
import java.util.Properties;
import java.util.Map.Entry;


/**
 * @author bce
 */
public class PropertiesHelper
{
  private static final char ENTRY_SEPARATOR = '=';

  private PropertiesHelper()
  {
  // wird nicht instantiiert
  }

  public static Properties parseFromString( final String source, final char separator )
  {
    final Properties props = new Properties();

    final String[] strings = source.split( "" + separator );
    for( int i = 0; i < strings.length; i++ )
    {
      int pos = strings[i].indexOf( ENTRY_SEPARATOR );
      if( pos > 0 )
        props.put( strings[i].substring( 0, pos ), strings[i].substring( pos + 1 ) );
    }

    return props;
  }

  public static String format( final Properties source, final char separator )
  {
    final StringBuffer sb = new StringBuffer();
    for( Iterator pIt = source.entrySet().iterator(); pIt.hasNext(); )
    {
      final Map.Entry entry = (Entry)pIt.next();

      sb.append( "" + entry.getKey() + ENTRY_SEPARATOR + entry.getValue() + separator );
    }

    return sb.toString();
  }

  /**
   * Macht ein Pattern-Matching auf allen Values einer Properties-Map und
   * ersetzt gemäss der Replace-Properties Map.
   */
  public static Properties replaceValues( final Properties sourceProps,
      final Properties replaceProperties )
  {
    final Properties newProps = new Properties();

    for( Iterator sourceIt = sourceProps.entrySet().iterator(); sourceIt.hasNext(); )
    {
      final Map.Entry sourceEntry = (Entry)sourceIt.next();
      final String sourceKey = sourceEntry.getKey().toString();
      final String sourceValue = sourceEntry.getValue().toString();

      final String newValue = StringUtilities.replaceAll( sourceValue, replaceProperties );
      newProps.setProperty( sourceKey, newValue );
    }

    return newProps;
  }

}