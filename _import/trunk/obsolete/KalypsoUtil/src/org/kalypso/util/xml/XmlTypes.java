package org.kalypso.util.xml;

import org.kalypso.util.parser.impl.DateParser;
import org.kalypso.util.parser.impl.DoubleParser;

/**
 * Utility class that contains the default java formaters for the types found in
 * xml.
 * 
 * @author schlienger
 */
public class XmlTypes
{
  private XmlTypes( )
  {
    // do not instanciate
  }

  /**
   * Parser for the type <code>date</code>. It uses following format string:
   * 
   * <pre>
   * yyyy-MM-dd'T'HH:mm:ss
   * </pre>
   */
  public final static DateParser PDATE = new DateParser(
      "yyyy-MM-dd'T'HH:mm:ss" );

  /**
   * Parser for the type <code>double</code>. It uses the default behaviour
   * of the java.lang.Double class.
   */
  public final static DoubleParser PDOUBLE = new DoubleParser();
}
