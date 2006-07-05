/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.sensor.timeseries;

import java.awt.Color;
import java.io.IOException;
import java.io.InputStream;
import java.text.DateFormat;
import java.text.NumberFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.ArrayUtils;
import org.kalypso.commons.java.util.StringUtilities;
import org.kalypso.contribs.java.awt.ColorUtilities;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.impl.DefaultAxis;
import org.kalypso.ogc.sensor.impl.SimpleObservation;
import org.kalypso.ogc.sensor.impl.SimpleTuppleModel;
import org.kalypso.ogc.sensor.timeseries.wq.IWQConverter;
import org.kalypso.ogc.sensor.timeseries.wq.WQException;
import org.kalypso.ogc.sensor.timeseries.wq.WQFactory;

/**
 * Utilities when dealing with Observations which are Kalypso Timeseries.
 * 
 * @author schlienger
 */
public class TimeserieUtils
{
  private static Properties m_config;

  private static HashMap m_formatMap = new HashMap();

  private static NumberFormat m_defaultFormat = null;

  private static DateFormat DF = new SimpleDateFormat( "dd.MM.yy HH:mm" );

  private TimeserieUtils()
  {
  // no instanciation
  }

  /**
   * Finds out which metadata of the given observation begin with the given prefix.
   * <p>
   * This is for instance usefull for the Alarmstufen
   * 
   * @param obs
   * @param mdPrefix
   * @return list of metadata keys or empty array if nothing found
   */
  public final static String[] findOutMDBeginningWith( final IObservation obs, final String mdPrefix )
  {
    if( obs == null )
      return ArrayUtils.EMPTY_STRING_ARRAY;

    final MetadataList mdl = obs.getMetadataList();

    final ArrayList mds = new ArrayList();

    final Iterator it = mdl.keySet().iterator();
    while( it.hasNext() )
    {
      final String md = it.next().toString();

      if( md.startsWith( mdPrefix ) )
        mds.add( md );
    }

    return (String[])mds.toArray( new String[mds.size()] );
  }

  /**
   * Finds out the list of alarmstufen metadata keys
   * 
   * @return list of metadata keys
   */
  public final static String[] findOutMDAlarmLevel( final IObservation obs )
  {
    return findOutMDBeginningWith( obs, "Alarmstufe" );
  }

  /**
   * Returns the color to use when displaying the value of the given Alarmstufe.
   * 
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
  private static Properties getProperties()
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
   * Sets the 'forecast' metadata of the given observation using the given date range. If from or to are null, does
   * nothing.
   */
  public final static void setForecast( final IObservation obs, final Date from, final Date to )
  {
    if( from != null && to != null )
    {
      obs.getMetadataList().setProperty( TimeserieConstants.MD_VORHERSAGE,
          TimeserieConstants.DEFAULT_DF.format( from ) + ";" + TimeserieConstants.DEFAULT_DF.format( to ) );
    }
  }

  /**
   * Returns a new instance of DateRangeArgument containing the beginning and the end of the forecast, given the
   * observation is a forecast.
   * <p>
   * An observation is a forecast when it has the MD_VORHERSAGE Metadata.
   * 
   * @param obs
   * @return date range of the forecast or null if obs isn't a forecast.
   */
  public final static DateRange isForecast( final IObservation obs )
  {
    if( obs == null )
      return null;

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

          return new DateRange( from, to );
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
   * Returns a user-friendly name for the given type.
   * 
   * <p>
   * Note to Developer: keep the config.properties file up-to-date
   * 
   * @return corresponding name (user friendly)
   */
  public static String getName( final String type )
  {
    final String strName = getProperties().getProperty( "AXISNAME_" + type, "" );

    return strName;
  }

  /**
   * Returns a color for the given type.
   * 
   * <p>
   * Note to Developer: keep the config.properties file up-to-date
   * 
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
   * <p>
   * Transforms physical units to TYPE-Constant used in Axis (best guess)
   * </p>
   * <p>
   * Uses UNIT_TO_TYPE_ Keys in config.properties
   * </p>
   * 
   * @param unit
   * @return type
   */
  public static String getTypeForUnit( final String unit )
  {
    return getProperties().getProperty( "UNIT_TO_TYPE_" + unit, "" );
  }

  /**
   * @return true if the axis type is known to be a key axis
   */
  public static boolean isKey( final String type )
  {
    return Boolean.valueOf( getProperties().getProperty( "IS_KEY_" + type, "false" ) ).booleanValue();
  }

  /**
   * Create a default axis for the given type.
   */
  public static IAxis createDefaulAxis( final String type )
  {
    return new DefaultAxis( getName( type ), type, getUnit( type ), getDataClass( type ), isKey( type ) );
  }

  /**
   * Create a default axis for the given type and the key flag.
   */
  public static IAxis createDefaulAxis( final String type, final boolean isKey )
  {
    return new DefaultAxis( getName( type ), type, getUnit( type ), getDataClass( type ), isKey );
  }

  /**
   * Returns a NumberFormat instance according to the given timeserie type. If there is no specific instance for the
   * given type, then a default number format is returned.
   * 
   * @return instance of NumberFormat that can be used to display the values to the user
   */
  public static NumberFormat getNumberFormatFor( final String type )
  {
    return getNumberFormat( getDefaultFormatString( type ) );
  }

  /**
   * Returns the adequate NumberFormat for the given format-string. It currently only supports formats of the form %X.Yf
   * where actually only the Y is used to build a NumberFormat with Y minimum/maximum-fraction-digits.
   * <p>
   * The plan is, once we'll be using JDK 5.0, we'll try to replace this with the built-in functionality provided with
   * formated printing.
   * <p>
   * TODO once on JDK 5.0 use formated printing if possible. Note that some refactoring might need to be done since we
   * currently work with NumberFormats.
   */
  public static NumberFormat getNumberFormat( final String format )
  {
    final NumberFormat nf = (NumberFormat)m_formatMap.get( format );
    if( nf != null )
      return nf;

    // parse the format spec and only take the min-fraction-digit part
    final String regex = "%([0-9]*)\\.?([0-9]*)f";
    final Pattern pattern = Pattern.compile( regex );
    final Matcher matcher = pattern.matcher( format );
    if( matcher.matches() )
    {
      final String minfd = matcher.group( 2 );

      final NumberFormat wf = NumberFormat.getInstance();
      final int intValue = Integer.valueOf( minfd ).intValue();
      wf.setMinimumFractionDigits( intValue );
      wf.setMaximumFractionDigits( intValue );
      m_formatMap.put( format, wf );

      return wf;
    }

    return getDefaultFormat();
  }

  private static NumberFormat getDefaultFormat()
  {
    if( m_defaultFormat == null )
    {
      m_defaultFormat = NumberFormat.getNumberInstance();
      m_defaultFormat.setMinimumFractionDigits( 3 );
    }

    return m_defaultFormat;
  }

  /**
   * It is currently fix and is: "dd.MM.yy HH:mm"
   * 
   * @return the date format to use when displaying dates for observations/timeseries
   */
  public static DateFormat getDateFormat()
  {
    return DF;
  }

  public static Class getDataClass( final String type )
  {
    try
    {
      return Class.forName( getProperties().getProperty( "AXISCLASS_" + type, "" ) );
    }
    catch( final ClassNotFoundException e )
    {
      throw new IllegalArgumentException( "Axis-type is not supported: " + type );
    }
  }

  public static IAxis[] createDefaultAxes( final String[] axisTypes, boolean firstWithKey )
  {
    final List axisList = new ArrayList();
    if( axisTypes != null && axisTypes.length > 0 )
    {
      axisList.add( TimeserieUtils.createDefaulAxis( axisTypes[0], firstWithKey ) );
      for( int i = 1; i < axisTypes.length; i++ )
        axisList.add( TimeserieUtils.createDefaulAxis( axisTypes[i], false ) );
    }
    return (IAxis[])axisList.toArray( new IAxis[axisList.size()] );
  }

  /**
   * @return the default format string for the given type
   */
  public static String getDefaultFormatString( final String type )
  {
    return getProperties().getProperty( "FORMAT_" + type );
  }

  /**
   * @return the default top margin defined for the given type or null if none
   */
  public static Double getTopMargin( final String type )
  {
    final String margin = getProperties().getProperty( "TOP_MARGIN_" + type );
    if( margin == null )
      return null;
    
    return Double.valueOf( margin );
  }
  
  /**
   * Create a test timeserie with a date axis and one default axis for each of the given axisTypes. A tupple-model is
   * randomly generated.
   * 
   * @param axisTypes
   *          as seen in TimeserieConstants.TYPE_*
   * @param amountRows
   *          amount of rows of the TuppleModel that is randomly created
   * @throws SensorException
   */
  public static IObservation createTestTimeserie( final String[] axisTypes, final int amountRows,
      final boolean allowNegativeValues ) throws SensorException
  {
    final IAxis[] axes = new IAxis[axisTypes.length + 1];
    axes[0] = TimeserieUtils.createDefaulAxis( TimeserieConstants.TYPE_DATE, true );
    for( int i = 0; i < axisTypes.length; i++ )
      axes[i + 1] = TimeserieUtils.createDefaulAxis( axisTypes[i] );

    final SimpleObservation obs = new SimpleObservation( axes );
    final SimpleTuppleModel model = new SimpleTuppleModel( axes );

    final Calendar cal = Calendar.getInstance();
    for( int i = 0; i < amountRows; i++ )
    {
      final Object[] tupple = new Object[axes.length];
      tupple[0] = cal.getTime();

      for( int j = 1; j < tupple.length; j++ )
      {
        if( allowNegativeValues )
          tupple[j] = new Double( Math.random() * 100 * ( Math.random() > .5 ? 1 : -1 ) );
        else
          tupple[j] = new Double( Math.random() * 100 );
      }

      model.addTupple( tupple );

      cal.add( Calendar.DAY_OF_YEAR, 1 );
    }

    obs.setValues( model );

    return obs;
  }

  /**
   * @param gkr
   *          the Gausskrüger Rechtswert as string
   * @return the corresponding Gausskrüger Coordinate System Name
   */
  public static String getCoordinateSystemNameForGkr( final String gkr )
  {
    return getProperties().getProperty( "GK_" + gkr.substring( 0, 1 ), "<konnte nicht ermittelt werden>" );
  }

  /**
   * Return the value of the alarmLevel in regard to the given axisType. The alarm-levels are stored according to the
   * W-axis. If you want the value according to the Q-axis you should call this function with axisType = Q
   * 
   * @param axisType
   *          the type of the axis for which to convert the alarm-level
   * @throws WQException
   */
  public static Double convertAlarmLevel( final IObservation obs, final String axisType, final Double alarmLevel,
      final Date date ) throws SensorException, WQException
  {
    if( axisType.equals( TimeserieConstants.TYPE_WATERLEVEL ) )
      return alarmLevel;

    final IWQConverter converter = WQFactory.createWQConverter( obs );

    if( axisType.equals( TimeserieConstants.TYPE_RUNOFF ) || axisType.equals( TimeserieConstants.TYPE_VOLUME ) )
      return new Double( converter.computeQ( date, alarmLevel.doubleValue() ) );

    throw new WQException( "Kann die Alarmstufe nicht nach " + axisType + " konvertieren." );
  }
}