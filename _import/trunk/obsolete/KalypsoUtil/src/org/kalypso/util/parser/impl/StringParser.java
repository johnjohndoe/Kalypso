package org.kalypso.util.parser.impl;

import org.kalypso.util.parser.AbstractParser;
import org.kalypso.util.parser.ParserException;

/**
 * StringParser
 * 
 * @author schlienger
 */
public class StringParser extends AbstractParser
{
  /**
   * @see org.kalypso.util.parser.AbstractParser#toStringInternal(java.lang.Object)
   */
  public String toStringInternal( Object obj )
  {
    return obj.toString();
  }

  /**
   * @see org.kalypso.util.parser.IParser#getObjectClass()
   */
  public Class getObjectClass( )
  {
    return String.class;
  }

  /**
   * @see org.kalypso.util.parser.IParser#getFormat()
   */
  public String getFormat( )
  {
    return "";
  }

  /**
   * @see org.kalypso.util.parser.IParser#parse(java.lang.String)
   */
  public Object parse( String text ) throws ParserException
  {
    return text;
  }

  /**
   * @see org.kalypso.util.parser.IParser#compare(java.lang.String, java.lang.String)
   */
  public int compare( String value1, String value2 ) throws ParserException
  {
    if( value1.compareTo( value2 ) == 0 )
      return 0;
    
    if( value1.length() < value2.length() )
      return -1;
    
    return 1;
  }

  /**
   * @see org.kalypso.util.parser.IParser#compare(java.lang.Object, java.lang.Object)
   */
  public int compare( Object value1, Object value2 ) throws ParserException
  {
    return compare( value1.toString(), value2.toString() );
  }
}
