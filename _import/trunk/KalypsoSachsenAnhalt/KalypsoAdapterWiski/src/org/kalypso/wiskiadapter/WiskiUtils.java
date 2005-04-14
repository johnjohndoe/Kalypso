package org.kalypso.wiskiadapter;

import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

import org.apache.commons.io.IOUtils;
import org.kalypso.repository.RepositoryException;

/**
 * WiskiUtils
 * 
 * @author schlienger
 */
public final class WiskiUtils
{
  /** name of the property that delivers the group names */
  final static String PROP_GROUP_NAMES = "GROUP_NAMES";

  /**
   * name of the properties delivering the number of days in the past that can
   * be used as default date-range
   */
  final static String PROP_NUMBER_OF_DAYS = "NUMBER_OF_DAYS";

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
   * Uses the property file to convert the wiski type into the kalypso type
   * 
   * @param wiskiType
   * @return
   */
  public static String wiskiType2Kalypso( final String wiskiType )
  {
    try
    {
      final String type = getProperties().getProperty( "TYPE_" + wiskiType );

      if( type == null )
        throw new IllegalArgumentException( "Wiski-Typ nicht erkannt: "
            + wiskiType );

      return type;
    }
    catch( RepositoryException e )
    {
      e.printStackTrace();
      throw new IllegalArgumentException( e.getLocalizedMessage() );
    }
  }

  /**
   * Uses the property file to fetch the corresponding kalypso metadata name
   * 
   * @param wiskiLevel
   * @return
   */
  public static String wiskiMetadataName2Kalypso( final String wiskiName )
  {
    try
    {
      final String md = getProperties().getProperty( "MD_" + wiskiName );

      if( md == null )
        throw new IllegalArgumentException( "Wiski-Name nicht erkannt: "
            + wiskiName );

      return md;
    }
    catch( RepositoryException e )
    {
      e.printStackTrace();
      throw new IllegalArgumentException( e.getLocalizedMessage() );
    }
  }

  /**
   * Uses the property file to convert the wiski status into the kalypso one
   * 
   * @param wiskiStatus
   * @return
   */
  public static Integer wiskiStatus2Kalypso( final String wiskiStatus )
  {
    try
    {
      final String status = getProperties().getProperty(
          "STATUS_" + wiskiStatus );

      if( status == null )
        throw new IllegalArgumentException( "Wiski-Status nicht erkannt: "
            + wiskiStatus );

      return Integer.valueOf( status );
    }
    catch( RepositoryException e )
    {
      e.printStackTrace();
      throw new IllegalArgumentException( e.getLocalizedMessage() );
    }
  }
}
