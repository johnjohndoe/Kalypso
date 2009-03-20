/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.psiadapter.repository;

import java.io.IOException;
import java.io.InputStream;
import java.util.Calendar;
import java.util.Properties;
import java.util.TimeZone;

import org.apache.commons.io.IOUtils;
import org.kalypso.commons.conversion.units.IValueConverter;
import org.kalypso.commons.conversion.units.KelvinCelsiusConverter;
import org.kalypso.commons.conversion.units.NoConverter;
import org.kalypso.commons.conversion.units.SIConverter;
import org.kalypso.contribs.java.util.CalendarUtilities;
import org.kalypso.psiadapter.PSICompactFactory;

import de.psi.go.lhwz.PSICompact;

/**
 * All acces to PSICompact properties via this class.
 * 
 * @author Gernot Belger
 *
 */
public class PSICompactConfig
{
  private final static String CONFIG = "/org/kalypso/psiadapter/resources/config.ini";

  private final static Integer ZERO = new Integer( 0 );

  /**
   * Kennzeichen der Zeitzone in welche PSICompact operiert
   */
  private static final String TIMEZONE_ID = "TIMEZONE_ID";

  /**
   * der Kalender-Field im Sinne von java.util.Calendar worauf die AMOUNT_BEFORE und AMOUNT_AFTER sich beziehen.
   */
  private static final String OVERWRITE_CALENDAR_FIELD = "OVERWRITE_CALENDAR_FIELD";

  /**
   * Anzahl an Zeit-Einheiten die vor der Begin einer Zeitreihe mit OVERWRITE_VALUE überschrieben werden, bevor die
   * Zeitreihe in PSICompact zurückgeschrieben wird
   */
  private static final String OVERWRITE_AMOUNT_BEFORE = "OVERWRITE_AMOUNT_BEFORE";

  /**
   * Anzahl an Zeit-Einheiten die nach der Ende einer Zeitreihe mit OVERWRITE_VALUE überschrieben werden, bevor die
   * Zeitreihe in PSICompact zurückgeschrieben wird
   */
  private static final String OVERWRITE_AMOUNT_AFTER = "OVERWRITE_AMOUNT_AFTER";

  /**
   * Der Step, mit welchem die Vorhersage rausgeschrieben wird Die Einheit ist OVERWRITE_CALENDAR_FIELD
   */
  private static final String OVERWRITE_STEP = "OVERWRITE_STEP";

  /**
   * Überschreibungswert um die Zeitreihe so zu markieren, dass keine Darstellung der entsprechende Werte bei
   * Betrachtung im Web (Informationsmanagementsystem) erfolgt
   */
  private static final String OVERWRITE_VALUE = "OVERWRITE_VALUE";

  private static final String NUMBER_OF_DAYS = "NUMBER_OF_DAYS";

  private static Properties m_factoryProperties = null;

  private static Calendar m_psiCalendar = null;

  private final static synchronized Properties getProperties( )
  {
    if( m_factoryProperties == null )
    {
      InputStream stream = null;

      try
      {
        m_factoryProperties = new Properties();

        stream = PSICompactFactory.class.getResourceAsStream( CONFIG );
        m_factoryProperties.load( stream );
        stream.close();
      }
      catch( IOException e )
      {
        e.printStackTrace();
        
        throw new IllegalStateException( "Error while creating PSICompact: " + e.toString() );
      }
      finally
      {
        IOUtils.closeQuietly( stream );
      }
    }

    return m_factoryProperties;
  }
  
  public final static String toKalypsoRight( final String psiRight )
  {
    return getProperties().getProperty( "RIGHT_" + psiRight );
  }

  public final static double getOverwriteValue( )
  {
    return Double.parseDouble( getProperties().getProperty( OVERWRITE_VALUE ) );
  }

  public final static int getOverwriteAmountBefore( )
  {
    return Integer.parseInt( getProperties().getProperty( OVERWRITE_AMOUNT_BEFORE ) );
  }

  public final static int getOverwriteAmountAfter( )
  {
    return Integer.parseInt( getProperties().getProperty( OVERWRITE_AMOUNT_AFTER ) );
  }

  public final static int getOverwriteStep( )
  {
    return Integer.parseInt( getProperties().getProperty( OVERWRITE_STEP ) );
  }

  public final static int getOverwriteCalendarField( )
  {
    return CalendarUtilities.getCalendarField( getProperties().getProperty( OVERWRITE_CALENDAR_FIELD ) );
  }

  public final static int getNumberOfDays( )
  {
    return Integer.valueOf( getProperties().getProperty( NUMBER_OF_DAYS, "100" ) );
  }
  
  public final static synchronized Calendar getCalendarForPSICompact( )
  {
    if( m_psiCalendar == null )
      m_psiCalendar = Calendar.getInstance( TimeZone.getTimeZone( getProperties().getProperty( TIMEZONE_ID ) ) );

    return m_psiCalendar;
  }
  
  /**
   * Converts the PSICompact-Status to the Kalypso internal BitMask.
   * 
   * @param status
   *          as delivered by PSICompact
   * @return an integer representing a bitmask.
   */
  public final static Integer psiStatusToMask( int status )
  {
    final Properties props = getProperties();

    switch( status )
    {
      case PSICompact.STATUS_AUTO:
        return Integer.valueOf( props.getProperty( "BM_" + "STATUS_AUTO" ) );
      case PSICompact.STATUS_ERSALLG:
        return Integer.valueOf( props.getProperty( "BM_" + "STATUS_ERSALLG" ) );
      case PSICompact.STATUS_MANKOR:
        return Integer.valueOf( props.getProperty( "BM_" + "STATUS_MANKOR" ) );
      case PSICompact.STATUS_NACH:
        return Integer.valueOf( props.getProperty( "BM_" + "STATUS_NACH" ) );
      case PSICompact.STATUS_NORM:
        return Integer.valueOf( props.getProperty( "BM_" + "STATUS_NORM" ) );
      case PSICompact.STATUS_NORMALLG:
        return Integer.valueOf( props.getProperty( "BM_" + "STATUS_NORMALLG" ) );
      case PSICompact.STATUS_OK:
        return Integer.valueOf( props.getProperty( "BM_" + "STATUS_OK" ) );
      case PSICompact.STATUS_REKO:
        return Integer.valueOf( props.getProperty( "BM_" + "STATUS_REKO" ) );
      case PSICompact.STATUS_UNDEF:
        return Integer.valueOf( props.getProperty( "BM_" + "STATUS_UNDEF" ) );

      default:
        return ZERO;
    }
  }

  /**
   * Helper that translates the measure type into a string label.
   * 
   * @param measType
   * @return label
   */
  public final static String measureTypeToString( int measType )
  {
    final Properties props = getProperties();

    switch( measType )
    {
      case PSICompact.MEAS_FLOW:
        return props.getProperty( "MEAS_FLOW" );
      case PSICompact.MEAS_LEVEL:
        return props.getProperty( "MEAS_LEVEL" );
      case PSICompact.MEAS_RAINFALL:
        return props.getProperty( "MEAS_RAINFALL" );
      case PSICompact.MEAS_TEMPERATUR:
        return props.getProperty( "MEAS_TEMPERATUR" );
      case PSICompact.MEAS_UNDEF:
        return props.getProperty( "MEAS_UNDEF" );
      default:
        return props.getProperty( "UNKNOWN" );
    }
  }

  /**
   * Helper für die Übersetzung des valueType in eine leesbare String
   */
  public final static String valueTypeToString( int valueType )
  {
    final Properties props = getProperties();

    switch( valueType )
    {
      case PSICompact.TYPE_MEASUREMENT:
        return props.getProperty( "TYPE_MEASUREMENT" );
      case PSICompact.TYPE_VALUE:
        return props.getProperty( "TYPE_VALUE" );
      case PSICompact.TYPE_UNDEF:
        return props.getProperty( "TYPE_UNDEF" );
      default:
        return props.getProperty( "UNKNOWN" );
    }
  }
  /**
   * Helper für die Übersetzung des 'Unit' (ObjectMetaData) in eine leesbare String
   */
  private final static String unitToString( final int unit )
  {
    final Properties props = getProperties();

    switch( unit )
    {
      case PSICompact.SI_CUBIC_METER_PER_SECOND:
        return props.getProperty( "SI_CUBIC_METER_PER_SECOND" );
      case PSICompact.SI_KELVIN:
        return props.getProperty( "SI_KELVIN" );
      case PSICompact.SI_METER:
        return props.getProperty( "SI_METER" );
      case PSICompact.SI_QUBIC_METER:
        return props.getProperty( "SI_QUBIC_METER" );
      case PSICompact.SI_NO_UNIT:
        return props.getProperty( "SI_NO_UNIT" );
      case PSICompact.SI_UNDEF:
        return props.getProperty( "SI_UNDEF" );
      default:
        return props.getProperty( "UNKNOWN" );
    }
  }
  
  /**
   * Creates the adequate converter between psi and kalypso units
   * 
   * @return adequate converter
   */
  public final static IValueConverter getConverter( final int psiUnit, final String kalypsoUnit )
  {
    switch( psiUnit )
    {
      case PSICompact.SI_KELVIN:
        return KelvinCelsiusConverter.getInstance();

      case PSICompact.SI_CUBIC_METER_PER_SECOND:
      case PSICompact.SI_METER:
      case PSICompact.SI_QUBIC_METER:
      {
        final String strPsiUnit = unitToString( psiUnit );

        if( !strPsiUnit.equals( kalypsoUnit ) )
          return new SIConverter( strPsiUnit, kalypsoUnit );
      }

      case PSICompact.SI_NO_UNIT:
      case PSICompact.SI_UNDEF:
      default:
      {
        // empty
      }
    }

    return NoConverter.getInstance();
  }
}
