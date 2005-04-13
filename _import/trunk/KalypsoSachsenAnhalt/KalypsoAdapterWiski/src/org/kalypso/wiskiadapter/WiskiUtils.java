package org.kalypso.wiskiadapter;

import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;

import org.apache.commons.io.IOUtils;
import org.kalypso.repository.RepositoryException;

import de.kisters.wiski.webdataprovider.common.net.KiWWDataProviderInterface;

/**
 * WiskiUtils
 * 
 * @author schlienger
 */
public final class WiskiUtils
{
  /** name of the property that delivers the group names */
  final static String CONFIG_GROUP_NAMES = "GROUP_NAMES";

  private static Properties PROPS = null;

  private WiskiUtils( )
  {
    // not to be instanciated
  }

  /**
   * Reads the properties from the configuration file in resources/config.ini
   * 
   * @throws RepositoryException
   */
  public static Properties getProperties( ) throws RepositoryException
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
      throw new RepositoryException( e );
    }
    finally
    {
      IOUtils.closeQuietly( ins );
    }
  }

  /**
   * Forces the properties to be reloaded for the next call to getProperties()
   */
  public static void forcePropertiesReload( )
  {
    PROPS = null;
  }

  /**
   * Parse the commonInfoList type as defined by the WDP (which is a HashMap
   * with a specific construction). See WDP-Doc for more information on this
   * structure.
   * <p>
   * The parsing here only returns the list of values for the given column. Each
   * resultset is scanned and the value of the column is fetched in an array.
   * Finally the array is returned.
   * 
   * @param commonInfoList
   * @return
   */
  public static String[] parseCommonInfoList( final HashMap commonInfoList,
      final String columnName )
  {
    final List resultList = (List) commonInfoList
        .get( KiWWDataProviderInterface.KEY_RESULT_LIST );

    final String[] results = new String[resultList.size()];
    int i = 0;
    for( final Iterator it = resultList.iterator(); it.hasNext(); )
    {
      final HashMap map = (HashMap) it.next();
      results[i++] = (String) map.get( columnName );
    }

    return results;
  }
}
