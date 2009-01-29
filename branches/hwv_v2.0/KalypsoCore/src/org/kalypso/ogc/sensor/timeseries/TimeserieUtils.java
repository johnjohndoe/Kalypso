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
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.text.DateFormat;
import java.text.NumberFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Properties;
import java.util.Set;
import java.util.TimeZone;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.ArrayUtils;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.kalypso.commons.java.util.StringUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.java.awt.ColorUtilities;
import org.kalypso.contribs.java.util.PropertiesUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.core.i18n.Messages;
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
import org.kalypsodeegree.KalypsoDeegreePlugin;

/**
 * Utilities when dealing with Observations which are Kalypso Timeseries.
 * 
 * @author schlienger
 */
public class TimeserieUtils
{
  private static final String PROP_TIMESERIES_CONFIG = "kalypso.timeseries.properties";

  private static URL m_configBaseUrl = TimeserieUtils.class.getResource( "resource/" ); //$NON-NLS-1$

  private static String m_basename = "config"; //$NON-NLS-1$

  private static Properties m_config;

  private static HashMap<String, NumberFormat> m_formatMap = new HashMap<String, NumberFormat>();

  private static NumberFormat m_defaultFormat = null;

  /**
   * Used by the ObservationTable and the observationDiagram
   */
  private static DateFormat DF = new SimpleDateFormat( "dd.MM.yy HH:mm" );

  static
  {
    final TimeZone timeZone;
    // if the platform is runnning, use its time zone
    if( Platform.isRunning() )
    {
      // Set the time zone according to the global settings
      timeZone = KalypsoCorePlugin.getDefault().getTimeZone();
      DF.setTimeZone( timeZone );
    }
    else
      timeZone = TimeZone.getTimeZone( "UTC" );
  }

  private TimeserieUtils( )
  {
    // no instanciation
  }

  /**
   * Allows to overwrite the location of the config.properties file.<br>
   * If international alternatives are present these will be used (i.e. config_de.properties instead of
   * config.properties).
   * 
   * @param configUrl
   *            Base location of the config file(s) (i.e. getClass().getResource("resources")).
   * @param basename
   *            base name of the config file (i.e. "config")
   */
  public static void setConfigUrl( final URL configUrl, final String basename )
  {
    m_configBaseUrl = configUrl;
    m_basename = basename;
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

    final ArrayList<String> mds = new ArrayList<String>();


    final Set<Object> keySet = mdl.keySet();
    for( final Object object : keySet )
    {
      final String md = object.toString();

      if( md.startsWith( mdPrefix ) )
        mds.add( md );
    }

    return mds.toArray( new String[mds.size()] );
  }

  /**
   * Finds out the list of alarmstufen metadata keys
   * 
   * @return list of metadata keys
   */
  public final static String[] findOutMDAlarmLevel( final IObservation obs )
  {
    return findOutMDBeginningWith( obs, Messages.getString("org.kalypso.ogc.sensor.timeseries.TimeserieUtils.2") ); //$NON-NLS-1$
  }

  /**
   * Returns the color to use when displaying the value of the given Alarmstufe.
   * 
   * @return color
   */
  public final static Color getColorForAlarmLevel( final String mdAlarm )
  {
    final String strColor = getProperties().getProperty( Messages.getString("org.kalypso.ogc.sensor.timeseries.TimeserieUtils.3") + mdAlarm ); //$NON-NLS-1$
    if( strColor == null )
      return Color.RED;

    return StringUtilities.stringToColor( strColor );
  }

  /**
   * Lazy loading of the properties
   * 
   * @return config of the timeseries package
   */
  private static synchronized Properties getProperties( )
  {
    if( m_config == null )
    {
      m_config = new Properties();

      final Properties defaultConfig = new Properties();
      m_config = new Properties( defaultConfig );

      // The config file in the sources is used as defaults
      PropertiesUtilities.loadI18nProperties( defaultConfig, m_configBaseUrl, m_basename );

      // TODO: also load configured properties via i18n mechanism
      InputStream configIs = null;
      try
      {
        // If we have a configured config file, use it as standard
        final URL configUrl = Platform.isRunning() ? Platform.getConfigurationLocation().getURL() : null;
        final String timeseriesConfigLocation = System.getProperty( PROP_TIMESERIES_CONFIG );
        final URL timeseriesConfigUrl = timeseriesConfigLocation == null ? null : new URL( configUrl,
            timeseriesConfigLocation );

        try
        {
          if( timeseriesConfigUrl != null )
            configIs = timeseriesConfigUrl.openStream();
        }
        catch( final FileNotFoundException ioe )
        {
          // ignore: there is no config file; we are using standard instead
          final IStatus status = StatusUtilities.createStatus( IStatus.WARNING, "Specified timeseries config file at "
              + timeseriesConfigUrl.toExternalForm() + " does not exist. Using default settings.", null );
          KalypsoCorePlugin.getDefault().getLog().log( status );
        }

        if( configIs != null )
        {
          m_config.load( configIs );
          configIs.close();
        }
      }
      catch( final IOException e )
      {
        e.printStackTrace();
      }
      finally
      {
        IOUtils.closeQuietly( configIs );
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
      obs.getMetadataList().setProperty( TimeserieConstants.MD_VORHERSAGE, TimeserieConstants.DEFAULT_DF.format( from ) + ";" + TimeserieConstants.DEFAULT_DF.format( to ) ); //$NON-NLS-1$
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
      final String[] splits = range.split( ";" ); //$NON-NLS-1$
      if( splits.length == 2 )
      {
        try
        {
// TODO: this dateFormat depends on the locale. This results in problems reading ZML files created using a different
// locale
          final Date from = TimeserieConstants.DEFAULT_DF.parse( splits[0] );
          final Date to = TimeserieConstants.DEFAULT_DF.parse( splits[1] );

          return new DateRange( from, to );
        }
        catch( final Exception e )
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
    return getProperties().getProperty( "AXISUNIT_" + type, "" );
  }

  /**
   * Returns a user-friendly name for the given type.
   * <p>
   * Note to Developer: keep the config.properties file up-to-date
   * 
   * @return corresponding name (user friendly)
   */
  public static String getName( final String type )
  {
    return getProperties().getProperty( "AXISNAME_" + type, "" );
  }

  /**
   * Returns a color for the given type.
   * <p>
   * Note to Developer: keep the config.properties file up-to-date
   * 
   * @return a Color that is defined to be used with the given axis type, or a random color when no fits
   */
  public static Color[] getColorsFor( final String type )
  {
    final String strColor = getProperties().getProperty( "AXISCOLOR_" + type ); //$NON-NLS-1$

    if( strColor == null )
      return new Color[]
                       { ColorUtilities.random() };

    final String[] strings = strColor.split( "#" );
    if( strings.length == 0 )
      return new Color[]
                       { ColorUtilities.random() };

    final Color[] colors = new Color[strings.length];
    for( int i = 0; i < colors.length; i++ )
      colors[i] = StringUtilities.stringToColor( strings[i] );

    return colors;
  }

  /**
   * @param mdKey
   * @return color for the given Metadata information
   */
  public static Color getColorForMD( final String mdKey )
  {
    final String strColor = getProperties().getProperty( "MDCOLOR_" + mdKey ); //$NON-NLS-1$

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
    return getProperties().getProperty( "UNIT_TO_TYPE_" + unit, "" ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  /**
   * @return true if the axis type is known to be a key axis
   */
  public static boolean isKey( final String type )
  {
    return Boolean.valueOf( getProperties().getProperty( "IS_KEY_" + type, "false" ) ).booleanValue(); //$NON-NLS-1$ //$NON-NLS-2$
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
  public static synchronized NumberFormat getNumberFormat( final String format )
  {
    final NumberFormat nf = m_formatMap.get( format );
    if( nf != null )
      return nf;

    // parse the format spec and only take the min-fraction-digit part
    final String regex = "%([0-9]*)\\.?([0-9]*)f"; //$NON-NLS-1$
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

  private static synchronized NumberFormat getDefaultFormat( )
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

  public static Class<?> getDataClass( final String type )
  {
    try
    {
      return Class.forName( getProperties().getProperty( "AXISCLASS_" + type, "" ) ); //$NON-NLS-1$ //$NON-NLS-2$
    }
    catch( final ClassNotFoundException e )
    {
      throw new IllegalArgumentException( Messages.getString("org.kalypso.ogc.sensor.timeseries.TimeserieUtils.19") + type ); //$NON-NLS-1$
    }
  }

  public static IAxis[] createDefaultAxes( final String[] axisTypes, final boolean firstWithKey )
  {
    final List<IAxis> axisList = new ArrayList<IAxis>();
    if( axisTypes != null && axisTypes.length > 0 )
    {
      axisList.add( TimeserieUtils.createDefaulAxis( axisTypes[0], firstWithKey ) );
      for( int i = 1; i < axisTypes.length; i++ )
        axisList.add( TimeserieUtils.createDefaulAxis( axisTypes[i], false ) );
    }
    return axisList.toArray( new IAxis[axisList.size()] );
  }

  /**
   * @return the default format string for the given type
   */
  public static String getDefaultFormatString( final String type )
  {
    return getProperties().getProperty( "FORMAT_" + type ); //$NON-NLS-1$
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
   *            as seen in TimeserieConstants.TYPE_*
   * @param amountRows
   *            amount of rows of the TuppleModel that is randomly created
   * @throws SensorException
   */
  public static IObservation createTestTimeserie( final String[] axisTypes, final int amountRows, final boolean allowNegativeValues ) throws SensorException
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
          tupple[j] = new Double( Math.random() * 100 * (Math.random() > .5 ? 1 : -1) );
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
   *            the Gausskrüger Rechtswert as string
   * @return the corresponding Gausskrüger Coordinate System Name
   */
  public static String getCoordinateSystemNameForGkr( final String gkr )
  {
    final String crsName = getProperties().getProperty( "GK_" + gkr.substring( 0, 1 ), null ); //$NON-NLS-1$
    if( crsName == null )
      KalypsoDeegreePlugin.getDefault().getCoordinateSystem();

    return crsName;
  }

  /**
   * Return the value of the alarmLevel in regard to the given axisType. The alarm-levels are stored according to the
   * W-axis. If you want the value according to the Q-axis you should call this function with axisType = Q
   * 
   * @param axisType
   *            the type of the axis for which to convert the alarm-level
   * @throws WQException
   */
  public static Double convertAlarmLevel( final IObservation obs, final String axisType, final Double alarmLevel, final Date date ) throws SensorException, WQException
  {
    if( axisType.equals( TimeserieConstants.TYPE_WATERLEVEL ) )
      return alarmLevel;

    final IWQConverter converter = WQFactory.createWQConverter( obs );

    if( axisType.equals( TimeserieConstants.TYPE_RUNOFF ) || axisType.equals( TimeserieConstants.TYPE_VOLUME ) )
      return new Double( converter.computeQ( date, alarmLevel.doubleValue() ) );

    throw new WQException( Messages.getString("org.kalypso.ogc.sensor.timeseries.TimeserieUtils.22") + axisType + Messages.getString("org.kalypso.ogc.sensor.timeseries.TimeserieUtils.23") ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  /**
   * Returns the class name for the given axis-type. The class must inherit from
   * <code>org.jfree.chart.axis.ValueAxis</code>.
   * 
   * @return The class name for the given axis-type. The class must inherit from
   *         <code>org.jfree.chart.axis.ValueAxis</code>.
   */
  public static String getAxisClassFor( final String type )
  {
    return getProperties().getProperty( "AXISJFREECHARTCLASS_" + type, null );
  }
}