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
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
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

  private static HashMap m_formatMap = null;

  private static NumberFormat m_defaultFormat = null;

  private TimeserieUtils()
  {
  // no instanciation
  }

  /**
   * Finds out which metadata of the given observation begin with the given
   * prefix.
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

    return (String[])mds.toArray( new String[mds.size()] );
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
   * Returns the color to use when displaying the value of the given Alarmstufe.
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
   * Sets the 'forecast' metadata of the given observation using the given date
   * range.
   * 
   * @param obs
   * @param from
   * @param to
   */
  public final static void setForecast( final IObservation obs, final Date from, final Date to )
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
   * @return a Color that is defined to be used with the given axis type, or a
   *         random color when no fits
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
   * Returns a NumberFormat instance according to the given timeserie type. If
   * there is no specific instance for the given type, then a default number
   * format is returned.
   * 
   * @param type
   * @return instance of NumberFormat that can be used to display the values to
   *         the user
   */
  public static NumberFormat getNumberFormatFor( final String type )
  {
    final NumberFormat nf = (NumberFormat)getFormatMap().get( type );
    if( nf != null )
      return nf;

    return getDefaultFormat();
  }

  private static Map getFormatMap()
  {
    if( m_formatMap == null )
    {
      m_formatMap = new HashMap();

      // for W
      final NumberFormat wf = NumberFormat.getInstance();
      wf.setMinimumFractionDigits( Integer.valueOf( getProperties().getProperty( "MFD_" + TimeserieConstants.TYPE_WATERLEVEL ) ).intValue() );
      m_formatMap.put( TimeserieConstants.TYPE_WATERLEVEL, wf );

      // for N
      final NumberFormat nf = NumberFormat.getInstance();
      wf.setMinimumFractionDigits( Integer.valueOf( getProperties().getProperty( "MFD_" + TimeserieConstants.TYPE_RAINFALL ) ).intValue() );
      m_formatMap.put( TimeserieConstants.TYPE_RAINFALL, nf );
    }

    return m_formatMap;
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

  public static Class getDataClass( String type )
  {
    try
    {
      return Class.forName( getProperties().getProperty( "AXISCLASS_" + type, "" ) );
    }
    catch( ClassNotFoundException e )
    {
      return null;
    }
  }
}