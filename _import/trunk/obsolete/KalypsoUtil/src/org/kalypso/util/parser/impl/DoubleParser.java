package org.kalypso.util.parser.impl;

import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.text.ParseException;

import org.kalypso.util.parser.AbstractParser;
import org.kalypso.util.parser.ParserException;

/**
 * Ein Parser für Double und Float Objekte.
 * 
 * @author schlienger
 */
public class DoubleParser extends AbstractParser
{
  private final NumberFormat m_df;

  private final String m_format;

  /**
   * Default constructor: calls DoubleParser( "" )
   */
  public DoubleParser()
  {
    this( "" );
  }

  /**
   * @param format
   *          siehe Spezifikation in DecimalFormat
   * @see DecimalFormat
   */
  public DoubleParser( String format )
  {
    m_format = format;

    if( format.length() == 0 )
      m_df = NumberFormat.getInstance();
    else
      m_df = new DecimalFormat( format );
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
    return m_format;
  }

  /**
   * @throws ParserException
   * @see org.kalypso.util.parser.IParser#parse(java.lang.String)
   */
  public Object parse( String text ) throws ParserException
  {
    try
    {
      return m_df.parse( text );
    }
    catch( ParseException e )
    {
      throw new ParserException( e );
    }
  }

  /**
   * @see org.kalypso.util.parser.AbstractParser#toStringInternal(java.lang.Object)
   */
  public String toStringInternal( Object obj )
  {
    return m_df.format( obj );
  }
}