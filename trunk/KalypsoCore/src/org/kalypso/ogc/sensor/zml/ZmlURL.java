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
  private ZmlURL( )
  {
    // do not instanciate
  }
  
  /**
   * Returns only the identifier part of the zml url. The URL may contain a query part
   * which will be ignored by this convenience method.
   * 
   * @param url
   * @return only identifier
   */
  public static String getIdentifierPart( final URL url )
  {
    final String strUrl = url.toExternalForm();
    
    int ix = strUrl.indexOf( '?' );
    if( ix != -1 )
      return strUrl.substring( 0, ix );
    
    return strUrl;
  }

  /**
   * Checks if the string contains the from-to specification and eventually creates the
   * corresponding DateRangeArgument.
   * <pre>
   * The format of the from-to specification should be as follows:
   * 
   * ...&lt;from&gt;yyyy-MM-ddTHH:mm:ss&lt;from&gt;&lt;to&gt;yyyy-MM-ddTHH:mm:ss&lt;/to&gt;...
   * </pre> 
   * 
   * @param str
   * @return DateRangeArgument
   */
  public static DateRangeArgument checkDateRange( final String str )
  {
    final String TAG_FROM1 = "<from>";
    final String TAG_FROM2 = "</from>";
    final String TAG_TO1 = "<to>";
    final String TAG_TO2 = "</to>";

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
        dFrom = (Date) XmlTypes.PDATE.parse( from );
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
        dTo = (Date) XmlTypes.PDATE.parse( to );
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
