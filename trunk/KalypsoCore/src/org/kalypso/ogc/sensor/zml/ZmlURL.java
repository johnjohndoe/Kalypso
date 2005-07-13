/*
 * --------------- Kalypso-Header --------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------
 */
package org.kalypso.ogc.sensor.zml;

import java.net.URL;
import java.util.Calendar;

import org.apache.commons.lang.StringUtils;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.request.IRequest;
import org.kalypso.ogc.sensor.request.RequestFactory;
import org.kalypso.zml.request.RequestType;

/**
 * Provides utility methods for manipulating the URLs designed to be used as Zml-Identifiers between kalypso client and
 * server.
 * <p>
 * This utility class is not intended to be instanciated. Use its static methods.
 * 
 * @author schlienger (18.05.2005)
 */
public final class ZmlURL
{
  private ZmlURL()
  {
  // not intended to be instanciated
  }

  /**
   * Returns true if the given Zml-Url solely denotes a context. In that case the Zml-Url will not be parsed the usual
   * way.
   */
  public static boolean isUseAsContext( final URL zmlUrl )
  {
    return isUseAsContext( zmlUrl.toExternalForm() );
  }

  /**
   * Returns true if the given Zml-Url solely denotes a context. In that case the Zml-Url will not be parsed the usual
   * way.
   */
  public static boolean isUseAsContext( final String href )
  {
    final String test = href.toLowerCase();
    return test.indexOf( ZmlURLConstants.FRAGMENT_USEASCONTEXT ) != -1;
  }

  /**
   * Return true if the given id represents a server-side url. A server-side url begins with the server-specific
   * scheme-name.
   * 
   * @param href
   *          string representation of the zml url
   * @return true if server side
   * @see ZmlURLConstants#SCHEME_OCS
   */
  public static boolean isServerSide( final String href )
  {
    return href.startsWith( ZmlURLConstants.SCHEME_OCS );
  }

  /**
   * Add the kalypso-ocs-scheme part to the id (if not already present)
   */
  public static String addServerSideId( final String href )
  {
    String ssid = href;
    if( !isServerSide( ssid ) )
      ssid = ZmlURLConstants.SCHEME_OCS + ":" + ssid;

    return ssid;
  }

  /**
   * Remove the kalypso-ocs-scheme part
   */
  public static String removeServerSideId( final String href )
  {
    final String id = href.replaceFirst( ZmlURLConstants.SCHEME_OCS + ":", "" );

    return id;
  }

  /**
   * Returns only the identifier part of the zml url. The URL may contain a query part which will be ignored by this
   * convenience method.
   * 
   * @return only identifier part
   */
  public static String getIdentifierPart( final URL url )
  {
    return getIdentifierPart( url.toExternalForm() );
  }

  /**
   * Returns only the identifier part of the zml url. The URL may contain a query part which will be ignored by this
   * convenience method.
   * 
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
   */
  public static String getSchemePart( final URL url )
  {
    return getSchemePart( url.toExternalForm() );
  }

  /**
   * Returns the scheme part of the given url
   */
  public static String getSchemePart( final String strUrl )
  {
    // TODO test if this is correct since only the first ':' found
    // will be returned here. But the url might contain something like
    // foo:bar://stuff hence results might not be correct!!!
    int ix = strUrl.indexOf( ":" );
    if( ix != -1 )
      return strUrl.substring( 0, ix );

    return strUrl;
  }

//  /**
//   * Inserts the date range or replaces the one if existing. The date range is inserted in the from-to specification in
//   * the query part of the url.
//   * 
//   * @return string containing the given date range
//   */
//  public static String insertDateRange( final String str, final DateRange dra )
//  {
//    // first replace the date range spec (does nothing if not present)
//    String tmpUrl = str.replaceFirst( ZmlURLConstants.TAG_FROM1 + ".*" + ZmlURLConstants.TAG_FROM2, "" );
//    tmpUrl = tmpUrl.replaceFirst( ZmlURLConstants.TAG_TO1 + ".*" + ZmlURLConstants.TAG_TO2, "" );
//
//    String[] strs = tmpUrl.split( "\\?", 2 );
//
//    if( strs[0].startsWith( "<" ) || strs[0].startsWith( "&lt;" ) )
//      tmpUrl = "?" + strs[0] + buildDateRangeSpec( dra );
//    else
//      tmpUrl = strs[0] + '?' + buildDateRangeSpec( dra );
//
//    if( strs.length >= 2 )
//      tmpUrl += strs[1];
//
//    return tmpUrl;
//  }
//
//  /**
//   * Builds the String representation of the given date range. Constructs a simple XML representation in the format:
//   * 
//   * <pre>
//   *   	&lt;from&gt;yyyy-MM-ddTHH:mm:ss&lt;from&gt;&lt;to&gt;yyyy-MM-ddTHH:mm:ss&lt;/to&gt;
//   * </pre>
//   */
//  public static String buildDateRangeSpec( final DateRange dra )
//  {
//    final StringBuffer bf = new StringBuffer();
//
//    bf.append( ZmlURLConstants.TAG_FROM1 ).append( XmlTypes.PDATE.toString( dra.getFrom() ) ).append(
//        ZmlURLConstants.TAG_FROM2 );
//    bf.append( ZmlURLConstants.TAG_TO1 ).append( XmlTypes.PDATE.toString( dra.getTo() ) ).append(
//        ZmlURLConstants.TAG_TO2 );
//
//    return bf.toString();
//  }

//  /**
//   * Checks if the string contains the from-to specification and eventually creates the corresponding DateRangeArgument.
//   * 
//   * <pre>
//   *       The format of the from-to specification should be as follows:
//   *       
//   *       ...&lt;from&gt;yyyy-MM-ddTHH:mm:ss&lt;from&gt;&lt;to&gt;yyyy-MM-ddTHH:mm:ss&lt;/to&gt;...
//   * </pre>
//   */
//  public static DateRange checkDateRange( final String str )
//  {
//    String from = null;
//
//    int f1 = str.indexOf( ZmlURLConstants.TAG_FROM1 );
//    if( f1 != -1 )
//    {
//      int f2 = str.indexOf( ZmlURLConstants.TAG_FROM2 );
//
//      if( f2 != -1 )
//        from = str.substring( f1 + ZmlURLConstants.TAG_FROM1.length(), f2 );
//    }
//
//    String to = null;
//
//    int t1 = str.indexOf( ZmlURLConstants.TAG_TO1 );
//    if( t1 != -1 )
//    {
//      int t2 = str.indexOf( ZmlURLConstants.TAG_TO2 );
//
//      if( t2 != -1 )
//        to = str.substring( t1 + ZmlURLConstants.TAG_TO1.length(), t2 );
//    }
//
//    Date dFrom = null;
//    Date dTo = null;
//
//    if( from != null )
//    {
//      try
//      {
//        dFrom = (Date)XmlTypes.PDATE.parse( from );
//      }
//      catch( final ParserException e )
//      {
//        e.printStackTrace();
//      }
//    }
//
//    if( to != null )
//    {
//      try
//      {
//        dTo = (Date)XmlTypes.PDATE.parse( to );
//      }
//      catch( final ParserException e )
//      {
//        e.printStackTrace();
//      }
//    }
//
//    // no date found, return null
//    if( dFrom == null && dTo == null )
//      return null;
//
//    // at least from, to or both found, return new arg
//    return new DateRange( dFrom, dTo );
//  }

  /**
   * Insert the filter spec into the zml url. Return the newly build url string. The filter string should not contain
   * the %lt;filter/&gt; tags, this is automatically handled by this method.
   * 
   * @param href
   *          the zml url to update
   * @param filter
   *          the xml oriented filter specification
   */
  public static String insertFilter( final String href, final String filter )
  {
    if( filter == null || filter.length() == 0 )
      return href;

    // first replace the filter spec (does nothing if not present)
    String tmp = href.replaceFirst( ZmlURLConstants.TAG_FILTER1 + ".*" + ZmlURLConstants.TAG_FILTER2, "" );

    String[] strs = tmp.split( "\\?", 2 );

    if( strs[0].startsWith( "<" ) || strs[0].startsWith( "&lt;" ) )
      tmp = "?" + strs[0] + ZmlURLConstants.TAG_FILTER1 + filter + ZmlURLConstants.TAG_FILTER2;
    else
      tmp = strs[0] + '?' + ZmlURLConstants.TAG_FILTER1 + filter + ZmlURLConstants.TAG_FILTER2;

    if( strs.length >= 2 )
      tmp += strs[1];

    return tmp;
  }

  public static String insertRequest( final String str, final IRequest request ) throws SensorException
  {
    try
    {
      RequestType requestType = RequestFactory.parseRequest( str );
      if( requestType == null )
        requestType = RequestFactory.OF.createRequest();
      
      requestType.setName( request.getName() );
      requestType.setAxes( StringUtils.join( request.getAxisTypes(), "," ) );
      requestType.setStatusAxes( StringUtils.join( request.getAxisTypesWithStatus(), "," ) );
  
      if( request.getDateRange() != null )
      {
        final Calendar from = Calendar.getInstance();
        from.setTime( request.getDateRange().getFrom() );
        requestType.setDateFrom( from );
  
        final Calendar to = Calendar.getInstance();
        to.setTime( request.getDateRange().getTo() );
        requestType.setDateTo( to );
      }
      
      // first replace the date range spec (does nothing if not present)
      String tmpUrl = str.replaceFirst( ZmlURLConstants.TAG_REQUEST1 + ".*" + ZmlURLConstants.TAG_REQUEST2, "" );
  
      final String[] strs = tmpUrl.split( "\\?", 2 );
      if( strs[0].startsWith( "<" ) || strs[0].startsWith( "&lt;" ) )
        tmpUrl = "?" + strs[0];
      else
        tmpUrl = strs[0] + '?'; 

      final String xmlStr = RequestFactory.buildXmlString( requestType, false );
      tmpUrl += xmlStr;
      
      if( strs.length >= 2 )
        tmpUrl += strs[1];
  
      return tmpUrl;
    }
    catch( final Exception e )
    {
      throw new SensorException( e );
    }
  }
}
