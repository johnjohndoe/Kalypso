package org.kalypso.java.reflect;

import java.text.DateFormat;
import java.text.ParseException;
import java.util.Date;

/**
 * @author schlienger
 *
 */
public class ValueObjectFactory
{
  private static DateFormat df = null;
  
  private static DateFormat getDateFormat()
  {
    if( df == null )
      df = DateFormat.getDateInstance();
    
    return df;
  }
  
  /**
   * Creates an Object of Class c and assign it the value.
   * @throws ParseException
   */
  public static Object createObjectWithStringValue( Class c, String value ) throws ParseException
  {
    if( c == Date.class )
      return getDateFormat().parse( value );
    
    if( c == Integer.class )
      return Integer.valueOf( value );
      
    if( Double.class.isAssignableFrom( c ) )
        return Double.valueOf( value );
      
    return new String( value );
  }
}
