package org.kalypso.util.parser.impl;

import org.kalypso.util.parser.AbstractParser;
import org.kalypso.util.parser.ParserException;

/**
 * Ein Parser für Double und Float Objekte.
 * 
 * @author schlienger
 */
public class DoubleParser extends AbstractParser
{

  /**
   * Default constructor: calls DoubleParser( "" )
   */
  public DoubleParser()
  {
    this( "");
  }

  /**
   * @param format
   *          siehe Spezifikation in DecimalFormat
   * @see java.text.DecimalFormat
   */
  public DoubleParser( String format )
  {
    
    format.getClass();
    //
  }

  /**
   * @see org.kalypso.util.parser.IParser#getObjectClass()
   */
  public Class getObjectClass()
  {
    return Double.class;
  }

  /**
   * @see org.kalypso.util.parser.IParser#getFormat()
   */
  public String getFormat()
  {
    return "";
  }

  /**
   * @see org.kalypso.util.parser.IParser#parse(java.lang.String)
   */
  public Object parse( String text ) throws ParserException
  {
    try
    {
      return Double.valueOf(text);
    }
    catch( NumberFormatException e )
    {
      throw new ParserException( e );
    }
  }

  /**
   * @see org.kalypso.util.parser.AbstractParser#toStringInternal(java.lang.Object)
   */
  public String toStringInternal( Object obj )
  {
    return obj.toString();
  }
  
  /**
   * @see org.kalypso.util.parser.IParser#compare(java.lang.Object,
   *      java.lang.Object)
   */
  public int compare( Object value1, Object value2 ) throws ParserException
  {
    double n1 = ((Number) value1).doubleValue();
    double n2 = ((Number) value2).doubleValue();

    return Double.compare( n1, n2 );
  }
}