package org.kalypso.wiskiadapter;

import java.io.IOException;
import java.io.InputStream;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.Properties;
import java.util.TimeZone;

import org.apache.commons.io.IOUtils;

import de.kisters.wiski.webdataprovider.common.net.KiWWDataProviderInterface;

/**
 * WiskiUtils
 * 
 * @author schlienger
 */
public final class WiskiUtils implements IWiskiConstants
{
  // TODO: do not make it static...
  // TODO: move properties tuff into extra, non-static, class
  private static Properties PROPS = null;

  private static Calendar calSrc = Calendar.getInstance();

  private static Calendar calDest = Calendar.getInstance();

  private WiskiUtils( )
  {
    // not to be instantiated
  }

  /**
   * Convert a date from one timezone to another timezone<br>
   * REMARK: do not move this method into a common utility method (like DateUtilities), as such stuff should never be
   * done normally (java.lang.Date's are always UTC internally).
   */
  public final static Date convert( final Date d, final TimeZone source, final TimeZone dest )
  {
    calSrc.setTimeZone( source );
    calDest.setTimeZone( dest );

    calSrc.setTimeInMillis( d.getTime() );

    calDest.clear();
    calDest.set( calSrc.get( Calendar.YEAR ), calSrc.get( Calendar.MONTH ), calSrc.get( Calendar.DAY_OF_MONTH ), calSrc.get( Calendar.HOUR_OF_DAY ), calSrc.get( Calendar.MINUTE ), calSrc.get( Calendar.SECOND ) );

    return calDest.getTime();
  }

  /**
   * Reads the properties from the configuration file in resources/config.ini
   */
  private static Properties getProperties()
  {
    // lazy loading
    if( PROPS != null )
      return PROPS;

    final InputStream ins = WiskiRepository.class.getResourceAsStream( "/org/kalypso/wiskiadapter/resources/config.ini" );

    PROPS = new Properties();
    try
    {
      PROPS.load( ins );

      return PROPS;
    }
    catch( final IOException e )
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
  public static void forcePropertiesReload( )
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
    final List resultList = (List) commonInfoList.get( KiWWDataProviderInterface.KEY_RESULT_LIST );

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
      final HashMap map = (HashMap) it.next();
      results[i++] = (String) map.get( columnName );
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

  /**
   * @return flag indicating if the date conversion between wiski and kalypso needs to be done
   */
  public static boolean isConversionNeeded( final TsInfoItem item )
  {
    final int timeLevel = item.getWiskiTimeLevel();

    switch( timeLevel )
    {
      case 1:
      case 2:
      case 3:
      case 4:
      {
        final int valueType = item.getWiskiValueType();
        switch( valueType )
        {
          case 1:
          case 2:
          case 3:
          case 4:
            return true;
        }
      }
      default:
        return false;
    }
  }

  /**
   * @return flag indicating if the time value must be set explicitely for fetched wiski values
   */
  public static boolean needsTimeAdjustment( final TsInfoItem item )
  {
    final int timeLevel = item.getWiskiTimeLevel();
    /* At the moment, only dayly values get a explicit timestamp from wdp (wiskiBegin)*/
    return timeLevel == 1;
  }

  /**
   * units to take for tsinfo_distvalue
   * <ul>
   * <li>1: seconds
   * <li>2: minutes
   * <li>3: hours
   * <li>4: days
   * </ul>
   * 
   * @return corresponding field value as defined in java.util.Calendar
   */
  public static int getDistUnitCalendarField( final int tsinfo_distunit )
  {
    switch( tsinfo_distunit )
    {
      case 1:
        return Calendar.SECOND;
      case 2:
        return Calendar.MINUTE;
      case 3:
        return Calendar.HOUR_OF_DAY;
      case 4:
        return Calendar.DAY_OF_MONTH;
      default:
        throw new IllegalStateException( "Cannot translate Wiski property tsinfo_distunit into Calendar-Field" );
    }
  }

  /**
   * Used when converting wiski dates into kalypso dates
   * 
   * @return corresponding java.util.Calendar field that corresponds to the timelevel of the given TsInfo object
   */
  public static int getConversionCalendarField( final int tsinfo_timelevel )
  {
    switch( tsinfo_timelevel )
    {
      case 1:
        return Calendar.DAY_OF_MONTH;
      case 2:
        return Calendar.MONTH;
      case 3:
        return Calendar.YEAR;
      case 4:
        return Calendar.WEEK_OF_YEAR;
      default:
        throw new IllegalStateException( "unsupported time level" );
    }
  }
  

  /**
   * Sets all 'Missing' values to a given default value. Teh status is not changed.
   */
  public static void fillMissing( final LinkedList data, final Double fillValue )
  {
    for( final Iterator dataIter = data.iterator(); dataIter.hasNext(); )
    {
      final HashMap map = (HashMap) dataIter.next();
      final String status = (String) map.get( WISKI_DATA_AXIS_QUALITY );
      if( WISKI_STATUS_MISSING.equals( status ) )
        map.put( WISKI_DATA_AXIS_VALUE, fillValue );
    }
  }

  /**
   * (Linear-)interpolates missing values in wiski data. <br>
   */
  public static void interpolateMissing( final LinkedList data )
  {
    int lastValid = -1;
    for( int i = 0; i < data.size(); i++ )
    {
      final HashMap iMap = (HashMap) data.get( i );
      final String status = (String) iMap.get( WISKI_DATA_AXIS_QUALITY );
      if( !WISKI_STATUS_MISSING.equals( status ) )
      {
        fillMissingHole( lastValid, i, data );
        lastValid = i;
      }
    }
  }

  private static void fillMissingHole( final int from, final int to, final LinkedList filteredData )
  {
    if( from < 0 || to > filteredData.size() - 1 || to - from < 2 )
      return;

    final HashMap fromData = (HashMap) filteredData.get( from );
    final HashMap toData = (HashMap) filteredData.get( to );

    final double fromValue = ((Number) fromData.get( WISKI_DATA_AXIS_VALUE )).doubleValue();
    final double toValue = ((Number) toData.get( WISKI_DATA_AXIS_VALUE )).doubleValue();

    // Iterate over missing values (if we have no hole, this is the empty for-loop)
    for( int i = from + 1; i < to; i++ )
    {
      // REMARK: we assume, that WISKI always delivers equidistant values; so we just do
      // linear interpolation via the index.
      final double interpolatedValue = fromValue + (i - from) * (toValue - fromValue) / (to - from);

      final HashMap map = (HashMap) filteredData.get( i );
      map.put( WISKI_DATA_AXIS_VALUE, new Double( interpolatedValue ) );
    }
  }

  /**
   * Removes all 'Missing' values from start and end of the given list.
   */
  public static LinkedList trimMissing( final LinkedList data )
  {
    final LinkedList trimmedData = new LinkedList( data );

    /* Remove values missing from start of timeserie */
    for( final Iterator dataIter = trimmedData.iterator(); dataIter.hasNext(); )
    {
      final HashMap map = (HashMap) dataIter.next();
      final String status = (String) map.get( WISKI_DATA_AXIS_QUALITY );
      if( WISKI_STATUS_MISSING.equals( status ) )
        dataIter.remove();
      else
        break; // stop at first real good value
    }

    /* Remove values missing from end of timeserie */
    for( final ListIterator dataIter = trimmedData.listIterator( trimmedData.size() ); dataIter.hasPrevious(); )
    {
      final HashMap map = (HashMap) dataIter.previous();
      final String status = (String) map.get( WISKI_DATA_AXIS_QUALITY );
      if( WISKI_STATUS_MISSING.equals( status ) )
        dataIter.remove();
      else
        break; // stop at first real good value
    }

    return trimmedData;
  }
  
}
