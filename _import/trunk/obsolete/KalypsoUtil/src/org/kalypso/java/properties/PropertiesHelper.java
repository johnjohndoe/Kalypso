package org.kalypso.java.properties;

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
    final  Properties props = new Properties();
    
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
}
