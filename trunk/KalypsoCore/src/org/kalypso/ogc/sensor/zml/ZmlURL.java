package org.kalypso.ogc.sensor.zml;

import java.net.URL;
import java.util.Date;

import org.kalypso.util.parser.ParserException;
import org.kalypso.util.runtime.args.DateRangeArgument;
import org.kalypso.util.xml.XmlTypes;

/**
 * ZmlUrlParser
 * 
 * @author schlienger
 */
public class ZmlURL
{
  private final static String TAG_FROM1 = "<from>";

  private final static String TAG_FROM2 = "</from>";

  private final static String TAG_TO1 = "<to>";

  private final static String TAG_TO2 = "</to>";

  private ZmlURL()
  {
  // do not instanciate
  }

  /**
   * Returns only the identifier part of the zml url. The URL may contain a
   * query part which will be ignored by this convenience method.
   * 
   * @param url
   * @return only identifier part
   */
  public static String getIdentifierPart( final URL url )
  {
    return getIdentifierPart( url.toExternalForm() );
  }

  /**
   * Returns only the identifier part of the zml url. The URL may contain a
   * query part which will be ignored by this convenience method.
   * 
   * @param strUrl
   * @return only identifier part
   */
  public static String getIdentifierPart( final String strUrl )
  {
    int ix = strUrl.indexOf( '?' );
    if( ix != -1 )
      return strUrl.substring( 0, ix );

    return strUrl;
  }

  /**
   * Returns the scheme part of the given url
   * 
   * @param url
   * @return scheme
   */
  public static String getSchemePart( final URL url )
  {
    return getSchemePart( url.toExternalForm() );
  }

  /**
   * Returns the scheme part of the given url
   * 
   * @param strUrl
   * @return scheme
   */
  public static String getSchemePart( final String strUrl )
  {
    int ix = strUrl.indexOf( ":" );
    if( ix != -1 )
      return strUrl.substring( ix );

    return strUrl;
  }

  /**
   * Inserts the date range or replaces the one if existing. The date range is
   * inserted in the from-to specification in the query part of the url.
   * 
   * @param str
   * @param dra
   * @return string containing the given date range
   */
  public static String insertDateRange( final String str, final DateRangeArgument dra )
  {
    // first replace the date range spec (does nothing if not present)
    String tmpUrl = str.replaceFirst( TAG_FROM1 + ".*" + TAG_FROM2, "" );
    tmpUrl = tmpUrl.replaceFirst( TAG_TO1 + ".*" + TAG_TO2, "" );

    String[] strs = tmpUrl.split( "\\?", 2 );

    tmpUrl = strs[0] + '?' + buildDateRangeSpec( dra );
    if( strs.length >= 2 )
      tmpUrl += strs[1];

    return tmpUrl;
  }

  /**
   * Builds the String representation of the given date range. Constructs a
   * simple XML representation in the format:
   * 
   * <pre>
   * 
   *   	&lt;from&gt;yyyy-MM-ddTHH:mm:ss&lt;from&gt;&lt;to&gt;yyyy-MM-ddTHH:mm:ss&lt;/to&gt;
   *  
   * </pre>
   * 
   * @param dra
   * @return string
   */
  public static String buildDateRangeSpec( final DateRangeArgument dra )
  {
    final StringBuffer bf = new StringBuffer();

    bf.append( TAG_FROM1 ).append( XmlTypes.PDATE.toString( dra.getFrom() ) ).append( TAG_FROM2 );
    bf.append( TAG_TO1 ).append( XmlTypes.PDATE.toString( dra.getTo() ) ).append( TAG_TO2 );

    // TODO prüfen ob in Ordnung dass die < und > Zeichen mit Entities ersetzt
    // werden...
    return bf.toString();//.replaceAll( "<", "&lt;" ).replaceAll( ">", "&gt;"
    // );
  }

  /**
   * Checks if the string contains the from-to specification and eventually
   * creates the corresponding DateRangeArgument.
   * 
   * <pre>
   * 
   *       The format of the from-to specification should be as follows:
   *       
   *       ...&lt;from&gt;yyyy-MM-ddTHH:mm:ss&lt;from&gt;&lt;to&gt;yyyy-MM-ddTHH:mm:ss&lt;/to&gt;...
   *  
   * </pre>
   * 
   * @param str
   * @return DateRangeArgument
   */
  public static DateRangeArgument checkDateRange( final String str )
  {
    String from = null;

    int f1 = str.indexOf( TAG_FROM1 );
    if( f1 != -1 )
    {
      int f2 = str.indexOf( TAG_FROM2 );

      if( f2 != -1 )
        from = str.substring( f1 + TAG_FROM1.length(), f2 );
    }

    String to = null;

    int t1 = str.indexOf( TAG_TO1 );
    if( t1 != -1 )
    {
      int t2 = str.indexOf( TAG_TO2 );

      if( t2 != -1 )
        to = str.substring( t1 + TAG_TO1.length(), t2 );
    }

    Date dFrom = null;
    Date dTo = null;

    if( from != null )
    {
      try
      {
        dFrom = (Date)XmlTypes.PDATE.parse( from );
      }
      catch( ParserException e )
      {
        e.printStackTrace();
      }
    }

    if( to != null )
    {
      try
      {
        dTo = (Date)XmlTypes.PDATE.parse( to );
      }
      catch( ParserException e )
      {
        e.printStackTrace();
      }
    }

    // no date found, return null
    if( dFrom == null && dTo == null )
      return null;

    // at least from, to or both found, return new arg
    return new DateRangeArgument( dFrom, dTo );
  }
}