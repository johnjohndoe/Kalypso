package org.kalypso.util.properties;

import java.util.Properties;

/**
 * @author bce
 */
public class PropertiesHelper
{
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
      int pos = strings[i].indexOf( "=" );
      if( pos > 0 )
        props.put( strings[i].substring( 0, pos ), strings[i].substring( pos + 1 ) );
    }

    return props;
  }
}
