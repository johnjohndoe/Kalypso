/*--------------- Kalypso-Header ------------------------------------------

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

 --------------------------------------------------------------------------*/

package org.kalypso.wiskiadapter;

import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

/**
 * Helper class which converts kalypso times to and from wiski times.
 * 
 * @author Gernot Belger
 */
public class WiskiTimeConverter
{
  private final TimeZone m_tzWiski;

  /** flag indicating whether to convert the dates or not */
  private final boolean m_needsConversion;
  /** [can be null] when dateOffset is also null, no conversion takes place */
  private Calendar m_wiskiBegin = null;
  /** [can be null] when beginDate is also null, no conversion takes place */
  private int m_wiskiOffset = 0;
  /** the calendar field for which the dateOffset will be used */
  private int m_wiskiDateOffsetField = 0;
  /** used for date conversions between wiski and kalypso */
  private final Calendar m_cal;

  /**
   * @param tzSrc
   *          the wiski timezone
   *  
   */
  public WiskiTimeConverter( final TimeZone tzSrc, final TsInfoItem tsinfo )
  {
    m_tzWiski = tzSrc;

    m_needsConversion = WiskiUtils.isConversionNeeded( tsinfo );

    // TODO: Tageswerte: Zeitreihen mit Tagessummen können, im Gegensatz zu Hochauflösenden Zeitreihen,
    // Lücken enthalten. Dies führt später zu Fehlern, da ereignisbasiert gedacht wird.
    // Es müsste hier also zusätzlich noch mal auf das richtige Zeitraster interpoliert werden.

    if( m_needsConversion )
    {
      // init a calendar for the begin date, it will be used to fetch the date fields
      m_wiskiBegin = Calendar.getInstance( m_tzWiski );
      m_wiskiBegin.setTime( tsinfo.getWiskiBegin() );
      // offset is directly adapted to take care of kalypso conventions

      // TODO: um den offset richtig zu aktivieren: hier die +1 einkommentieren!
      m_wiskiOffset = tsinfo.getWiskiOffset().intValue() + 1;
      m_wiskiDateOffsetField = WiskiUtils.getConversionCalendarField( tsinfo.getWiskiTimeLevel() );
    }

    m_cal = Calendar.getInstance( m_tzWiski );
  }

  /**
   * In WISKI werden Tageswerte nicht richtig abgebildet (die Uhrzeit ist immer 00:00:00). Für die vollständige
   * Berücksichtigung der Anfangswerte und Offset, wurden zwei Zusatzfelder implementiert:
   * <ul>
   * <li>tsinfo_begin_of als timestamp, dessen Time-Anteil den Beginn der Integrationszeit des Tageswertes Beschreibt
   * (z.B. 07:30 oder ähnlich)
   * <li>tsinfo_offset_of als long, welcher beschreibt, ob die Quellwerte eines Tageswertes zum Datum x vom Tag x bis
   * x+1 einfliessen (offset 0) oder z.B. vom Tag x-1 bis zum Tag x (offset -1).
   * </ul>
   * 
   * Diese Methode passt das Datum so an, dass es die vollständige Information beinhaltet
   * 
   * @return das Datum nachdem der Tagesanfang und -Offset berücksichtigt wurde
   */
  public Date wiskiToKalypso( final Date d )
  {
    if( !m_needsConversion )
      return d;

    // Tageswert Problematik, Begin und Offset berücksichtigen
    m_cal.setTime( d );

    // Begin-Zeit setzen (Hour, Minute, Second)
    m_cal.set( Calendar.HOUR, m_wiskiBegin.get( Calendar.HOUR ) );
    m_cal.set( Calendar.MINUTE, m_wiskiBegin.get( Calendar.MINUTE ) );
    m_cal.set( Calendar.SECOND, m_wiskiBegin.get( Calendar.SECOND ) );

    // Offset berücksichtigen
    m_cal.add( m_wiskiDateOffsetField, m_wiskiOffset );

    return m_cal.getTime();
  }

  /**
   * Same as {@link #wiskiToKalypso(Date)}, but opposite direction (= negative offset)
   * 
   *@see WiskiTimeConverter#wiskiToKalypso(Date) 
   * @return das Datum nachdem der Tagesanfang und -Offset berücksichtigt wurde
   */
  public Date kalypsoToWiski( final Date d )
  {
    if( !m_needsConversion )
      return d;

    // Tageswert Problematik, Begin und Offset berücksichtigen
    m_cal.setTime( d );

    // Begin-Zeit setzen (Hour, Minute, Second)
    m_cal.set( Calendar.HOUR, m_wiskiBegin.get( Calendar.HOUR ) );
    m_cal.set( Calendar.MINUTE, m_wiskiBegin.get( Calendar.MINUTE ) );
    m_cal.set( Calendar.SECOND, m_wiskiBegin.get( Calendar.SECOND ) );

    // Offset berücksichtigen
    m_cal.add( m_wiskiDateOffsetField, -m_wiskiOffset );

    return m_cal.getTime();
  }

}
