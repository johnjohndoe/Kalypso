package org.kalypso.util.parser.impl;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

import org.kalypso.util.parser.AbstractParser;
import org.kalypso.util.parser.ParserException;

/**
 * Ein Parser für Date Objekte.
 * 
 * @author schlienger
 */
public class DateParser extends AbstractParser
{
  private final SimpleDateFormat m_df;

  private final String m_format;

  /**
   * Default constructor.
   */
  public DateParser()
  {
    this( "" );
  }

  /**
   * @param format
   *          siehe Spezifikation in SimpleDateFormat
   * @see SimpleDateFormat
   */
  public DateParser( String format )
  {
    m_format = format;

    if( format.length() == 0 )
      m_df = new SimpleDateFormat();
    else
      m_df = new SimpleDateFormat( format );
  }

  /**
   * @see org.kalypso.util.parser.IParser#getObjectClass()
   */
  public Class getObjectClass()
  {
    return Date.class;
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

  /**
   * @see org.kalypso.util.parser.IParser#compare(java.lang.Object,
   *      java.lang.Object)
   */
  public int compare( Object value1, Object value2 )
  {
    Date d1 = (Date) value1;
    Date d2 = (Date) value2;

    return d1.compareTo( d2 );
  }
}