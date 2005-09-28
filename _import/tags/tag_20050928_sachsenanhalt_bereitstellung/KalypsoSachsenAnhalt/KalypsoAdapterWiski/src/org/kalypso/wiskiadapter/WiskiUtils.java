package org.kalypso.wiskiadapter;

import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;

import org.apache.commons.io.IOUtils;

import de.kisters.wiski.webdataprovider.common.net.KiWWDataProviderInterface;

/**
 * WiskiUtils
 * 
 * @author schlienger
 */
public final class WiskiUtils
{
  /** name of the property that delivers the group names */
  final static String PROP_SUPERGROUPNAMES = "SUPERGROUPNAMES";

  /**
   * name of the properties delivering the number of days in the past that can be used as default date-range
   */
  final static String PROP_NUMBER_OF_DAYS = "NUMBER_OF_DAYS";

  private static Properties PROPS = null;

  private WiskiUtils()
  {
  // not to be instanciated
  }

  /**
   * Reads the properties from the configuration file in resources/config.ini
   */
  public static Properties getProperties()
  {
    // lazy loading
    if( PROPS != null )
      return PROPS;

    final InputStream ins = WiskiRepository.class
        .getResourceAsStream( "/org/kalypso/wiskiadapter/resources/config.ini" );

    PROPS = new Properties();
    try
    {
      PROPS.load( ins );

      return PROPS;
    }
    catch( IOException e )
    {
      e.printStackTrace();
      throw new IllegalStateException( e.getLocalizedMessage() );
    }
    finally
    {
      IOUtils.closeQuietly( ins );
    }
  }
  
  public static String getProperty( final String key )
  {
    return getProperties().getProperty( key );
  }
  
  public static String getProperty( final String key, final String defaultValue )
  {
    return getProperties().getProperty( key, defaultValue );
  }

  /**
   * Forces the properties to be reloaded for the next call to getProperties()
   */
  public static void forcePropertiesReload()
  {
    PROPS = null;
  }

  /**
   * Parse the commonInfoList type as defined by the WDP (which is a HashMap with a specific construction). See WDP-Doc
   * for more information on this structure.
   * <p>
   * The parsing here only returns the list of values for the given column. Each resultset is scanned and the value of
   * the column is fetched in an array. Finally the array is returned.
   */
  public static String[] parseCommonInfoList( final HashMap commonInfoList, final String columnName )
  {
    final List resultList = (List)commonInfoList.get( KiWWDataProviderInterface.KEY_RESULT_LIST );

    return parseResultList( resultList, columnName );
  }

  /**
   * Parse the resultList type as defined by the WDP (which is a List of HashMaps). See WDP-Doc for more information on
   * this structure.
   * <p>
   * The parsing here only returns the list of values for the given column. Each resultset is scanned and the value of
   * the column is fetched in an array. Finally the array is returned.
   */
  public static String[] parseResultList( final List resultList, final String columnName )
  {
    final String[] results = new String[resultList.size()];
    int i = 0;
    for( final Iterator it = resultList.iterator(); it.hasNext(); )
    {
      final HashMap map = (HashMap)it.next();
      results[i++] = (String)map.get( columnName );
    }

    return results;
  }

  /**
   * Uses the property file to convert the wiski type into the kalypso type
   */
  public static String wiskiType2Kalypso( final String wiskiType )
  {
    final String type = getProperty( "TYPE_" + wiskiType );

    if( type == null )
      throw new IllegalArgumentException( "Wiski-Typ nicht erkannt: " + wiskiType );

    return type;
  }

  /**
   * Uses the property file to fetch the corresponding kalypso metadata name
   */
  public static String wiskiMetadataName2Kalypso( final String wiskiName )
  {
    final String md = getProperty( "MD_" + wiskiName );

    if( md == null )
      throw new IllegalArgumentException( "Wiski-Name nicht erkannt: " + wiskiName );

    return md;
  }

  /**
   * Uses the property file to convert the wiski status into the kalypso one
   */
  public static Integer wiskiStatus2Kalypso( final String wiskiStatus )
  {
    final String status = getProperty( "STATUS_" + wiskiStatus );

    if( status == null )
      throw new IllegalArgumentException( "Wiski-Status nicht erkannt: " + wiskiStatus );

    return Integer.valueOf( status );
  }
}
