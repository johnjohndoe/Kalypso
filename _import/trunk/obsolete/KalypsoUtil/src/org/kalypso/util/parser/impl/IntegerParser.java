package org.kalypso.util.parser.impl;

import java.text.NumberFormat;
import java.text.ParseException;

import org.kalypso.util.parser.AbstractParser;
import org.kalypso.util.parser.ParserException;

/**
 * Ein Parser für Integer, Long, und Short Objekte.
 * 
 * @author schlienger
 */
public class IntegerParser extends AbstractParser
{
  private final NumberFormat m_nf;

  private final String m_format;

  /**
   * Default Constructor
   */
  public IntegerParser()
  {
    this( "" );
  }

  /**
   * @param format
   *          hat keine Bedeutung für Integers
   */
  public IntegerParser( String format )
  {
    m_format = format;

    m_nf = NumberFormat.getIntegerInstance();
  }

  /**
   * @see org.kalypso.util.parser.IParser#getObjectClass()
   */
  public Class getObjectClass()
  {
    return Integer.class;
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
      return m_nf.parse( text );
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
    return m_nf.format( obj );
  }
}