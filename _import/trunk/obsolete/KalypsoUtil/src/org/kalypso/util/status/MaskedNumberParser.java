package org.kalypso.util.status;

import org.kalypso.util.parser.ParserException;
import org.kalypso.util.parser.impl.DoubleParser;

/**
 * @author schlienger
 *
 */
public class MaskedNumberParser extends DoubleParser
{
  public MaskedNumberParser()
  {
    super();
  }

  public MaskedNumberParser( String format )
  {
    super( format );
  }

  /**
   * @see org.kalypso.util.parser.impl.DoubleParser#parse(java.lang.String)
   */
  public Object parse( String text ) throws ParserException
  {
    Number n = (Number)super.parse( text );
    
    // TODO: bit mask bit hier richtig setzen!
    return new MaskedNumber( n, 2 );
  }
  
  /**
   * @see org.kalypso.util.parser.AbstractParser#toString(java.lang.Object)
   */
  public String toString( Object obj ) throws ParserException
  {
    return super.toString( ((MaskedNumber)obj).getNumber() );
  }
  
  /**
   * @see org.kalypso.util.parser.impl.DoubleParser#getObjectClass()
   */
  public Class getObjectClass()
  {
    return MaskedNumber.class;
  }
}
