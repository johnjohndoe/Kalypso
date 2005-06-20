package org.kalypso.util.cache;

import java.text.ParseException;
import java.text.SimpleDateFormat;

/**
 * StringValidityKeyFactory
 * 
 * @author schlienger
 */
public class StringValidityKeyFactory implements IKeyFactory
{
  private final static String KEY_SEP = "@";

  private final static SimpleDateFormat DF = new SimpleDateFormat( "yyyy-MM-dd HH:mm:ss" );

  public Object createKey( final String string )
  {
    final String[] splits = string.split( KEY_SEP );
    try
    {
      final StringValidityKey key = new StringValidityKey( splits[0], DF.parse( splits[1] ) );
      return key;
    }
    catch( ParseException e )
    {
      e.printStackTrace();
      throw new IllegalArgumentException( e.getLocalizedMessage() );
    }
  }

  public String toString( final Object key )
  {
    final StringValidityKey rtkey = (StringValidityKey)key;
    final StringBuffer sb = new StringBuffer();
    sb.append( rtkey.getString() ).append( KEY_SEP ).append( DF.format( rtkey.getValidity() ) );

    return sb.toString();
  }
}
