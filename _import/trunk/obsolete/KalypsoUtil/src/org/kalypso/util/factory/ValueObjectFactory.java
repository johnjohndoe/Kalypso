package org.kalypso.util.factory;

import java.text.DateFormat;
import java.text.ParseException;
import java.util.Date;

/**
 * Eine Factory um Value Object anhand von strings zu erzeugen.<p>
 * 
 * TODO: die Möglichkeit lassen die formate von date, doubles, usw.
 * zu definieren. vielleicht in eine property datei.
 * 
 * @author schlienger
 */
public class ValueObjectFactory
{
  private static DateFormat df = null;
 
  private ValueObjectFactory()
  {
    // not intended to be instanciated
  }
  
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

  /**
   * Creates a string representation of the given object.
   */
  public static String toStringRepresentation( Object obj )
  {
    if( obj instanceof Date )
      return getDateFormat().format( obj );
    
    if( obj instanceof Integer )
      return obj.toString();
    
    if( obj instanceof Double )
      return obj.toString();
    
    return obj.toString();
  }
}
