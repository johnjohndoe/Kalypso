package org.kalypso.ogc.sensor.timeseries;

import java.awt.Color;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.Properties;

import org.apache.commons.io.IOUtils;
import org.kalypso.java.awt.ColorUtilities;
import org.kalypso.java.util.StringUtilities;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.util.runtime.args.DateRangeArgument;

/**
 * Utilities when dealing with Observations which are Kalypso Timeseries.
 * 
 * @author schlienger
 */
public class TimeserieUtils
{
  private static Properties m_config;

  private TimeserieUtils( )
  {
    // no instanciation
  }

  /**
   * Finds out which metadata of the given observation begin with the
   * given prefix.
   * <p>
   * This is for instance usefull for the Alarmstufen
   * 
   * @param obs
   * @param mdPrefix
   * @return list of metadata keys or empty array if nothing found
   */
  public final static String[] findOutMDBeginningWith( final IObservation obs, final String mdPrefix )
  {
    final MetadataList mdl = obs.getMetadataList();
    
    final ArrayList mds = new ArrayList();
    
    final Iterator it = mdl.keySet().iterator();
    while( it.hasNext() )
    {
      final String md = it.next().toString();
      
      if( md.startsWith( mdPrefix ) )
        mds.add( md );
    }
    
    return (String[]) mds.toArray( new String[mds.size()]);
  }

  /**
   * Finds out the list of alarmstufen metadata keys.
   * 
   * @param obs
   * @return list of metadata keys
   */
  public final static String[] findOutMDAlarmLevel( final IObservation obs )
  {
    return findOutMDBeginningWith( obs, "Alarmstufe" );
  }
  
  /**
   * Returns the color to use when displaying the value of the
   * given Alarmstufe.
   * 
   * @param mdAlarm
   * @return color
   */
  public final static Color getColorForAlarmLevel( final String mdAlarm )
  {
    final String strColor = getProperties().getProperty( "COLOR_" + mdAlarm );
    if( strColor == null )
      return Color.RED;
    
    return StringUtilities.stringToColor( strColor );
  }
  
  /**
   * Layze loading of the properties
   * 
   * @return config of the timeseries package
   */
  private static Properties getProperties( )
  {
    if( m_config == null )  
    {
      m_config = new Properties();
     
      InputStream ins = TimeserieUtils.class.getResourceAsStream( "resource/config.properties" );
      
      try
      {
        m_config.load( ins );
      }
      catch( IOException e )
      {
        e.printStackTrace();
      }
      finally
      {
        IOUtils.closeQuietly( ins );
      }
    }
    return m_config;
  }

  /**
   * Sets the 'forecast' metadata of the given observation using the given date
   * range.
   * 
   * @param obs
   * @param from
   * @param to
   */
  public final static void setForecast( final IObservation obs,
      final Date from, final Date to )
  {
    obs.getMetadataList().setProperty(
        TimeserieConstants.MD_VORHERSAGE,
        TimeserieConstants.DEFAULT_DF.format( from ) + ";"
            + TimeserieConstants.DEFAULT_DF.format( to ) );
  }

  /**
   * Returns a new instance of DateRangeArgument containing the beginning and
   * the end of the forecast, given the observation is a forecast.
   * <p>
   * An observation is a forecast when it has the MD_VORHERSAGE Metadata.
   * 
   * @param obs
   * @return date range of the forecast or null if obs isn't a forecast.
   */
  public final static DateRangeArgument isForecast( final IObservation obs )
  {
    final MetadataList mdl = obs.getMetadataList();
    final String range = mdl.getProperty( TimeserieConstants.MD_VORHERSAGE );
    if( range != null )
    {
      final String[] splits = range.split( ";" );
      if( splits.length == 2 )
      {
        try
        {
          final Date from = TimeserieConstants.DEFAULT_DF.parse( splits[0] );
          final Date to = TimeserieConstants.DEFAULT_DF.parse( splits[1] );

          return new DateRangeArgument( from, to );
        }
        catch( Exception e )
        {
          e.printStackTrace();
        }
      }
    }

    return null;
  }
  
  /**
   * Units are read from the config.properties file.
   * 
   * @param type
   * @return corresponding unit
   */
  public static String getUnit( final String type )
  {
    final String strUnit = getProperties().getProperty( "AXISUNIT_" + type, "" );

    return strUnit;
  }
  
  /**
   * TODO fill the names in the config.properties file
   * 
   * @param type
   * @return corresponding name (user friendly)
   */
  public static String getName( final String type )
  {
    final String strName = getProperties().getProperty( "AXISNAME_" + type, "" );

    return strName;
  }

  /**
   * @param type
   * @return a Color that is defined to be used with the given axis type, or a random color when no fits
   */
  public static Color getColorFor( final String type )
  {
    final String strColor = getProperties().getProperty( "AXISCOLOR_" + type );

    if( strColor != null )
      return StringUtilities.stringToColor( strColor );
    
    // no color found? so return random one
    return ColorUtilities.random();
  }

  /**
   * @param mdKey
   * @return color for the given Metadata information
   */
  public static Color getColorForMD( final String mdKey )
  {
    final String strColor = getProperties().getProperty( "MDCOLOR_" + mdKey );

    if( strColor != null )
      return StringUtilities.stringToColor( strColor );
    
    // no color found? so return random one
    return ColorUtilities.random();
  }

  /**
   * <p>Transforms physical units to TYPE-Constant used in Axis (best guess)</p>
   * <p>Uses UNIT_TO_TYPE_ Keys in config.properties</p>
   * 
   * @param unit
   */
  public static String getTypeForUnit( final String unit )
  {
    return getProperties().getProperty( "UNIT_TO_TYPE_" + unit, "" );
  }
}