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
 *  g.belger@bjoernsen.de
 *  m.schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.model.wavos;

import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;

import com.braju.format.Format;

/**
 * 
 * TODO: insert type comment here
 * 
 * @author thuel2
 */
public class WavosUtils
{
  /**
   * Gibt die aktuelle Uhrzeit zurück
   * 
   * @author Thül
   */
  public static String getAktuelleUhrzeit()
  {
    final GregorianCalendar gregCal;
    String sUhrzeit;

    gregCal = new GregorianCalendar();
    sUhrzeit = String.valueOf( gregCal.get( Calendar.DAY_OF_MONTH ) ) + "."
        + String.valueOf( gregCal.get( Calendar.MONTH ) + 1 ) + "." + String.valueOf( gregCal.get( Calendar.YEAR ) )
        + " " + String.valueOf( gregCal.get( Calendar.HOUR_OF_DAY ) ) + ":"
        + String.valueOf( gregCal.get( Calendar.MINUTE ) ) + ":" + String.valueOf( gregCal.get( Calendar.SECOND ) );

    return sUhrzeit;
  }

  public static Date addHour( final Date date, final int intervalAmount )
  {
    final Calendar c = Calendar.getInstance();
    c.setTime( date );
    c.add( Calendar.HOUR_OF_DAY, intervalAmount );
    return c.getTime();
  }

  /**
   * @param date
   * @throws NumberFormatException
   */
  public static String createInputFleName( final Date date ) throws NumberFormatException
  {
    final Calendar c = Calendar.getInstance();
    c.setTime( date );

    //TODO? Zeitversatz (Sommer/Winter)
    return Format.sprintf( WavosConst.FLUSS_INPUT + ".%04d%02d%02d%02d", new Object[]
    {
        Integer.valueOf( Integer.toString( c.get( Calendar.YEAR ) ) ),
        Integer.valueOf( Integer.toString( c.get( Calendar.MONTH ) + 1 ) ),
        Integer.valueOf( Integer.toString( c.get( Calendar.DAY_OF_MONTH ) ) ),
        Integer.valueOf( Integer.toString( c.get( Calendar.HOUR_OF_DAY ) ) ) } );

  }

  /**
   * @param date
   * @throws NumberFormatException
   */
  public static String createWavosDate( final Date date ) throws NumberFormatException
  {
    final Calendar c = Calendar.getInstance();
    c.setTime( date );
    return Format.sprintf( "%4d %02d %02d %02d %02d", new Object[]
    {
        Integer.valueOf( Integer.toString( c.get( Calendar.YEAR ) ) ),
        Integer.valueOf( Integer.toString( c.get( Calendar.MONTH ) + 1 ) ),
        Integer.valueOf( Integer.toString( c.get( Calendar.DAY_OF_MONTH ) ) ),
        Integer.valueOf( Integer.toString( c.get( Calendar.HOUR_OF_DAY ) ) ),
        Integer.valueOf( Integer.toString( c.get( Calendar.MINUTE ) ) ) } );
  }

  /**
   * @param date
   * @throws NumberFormatException
   */
  public static String createWavosDateShort( final Date date ) throws NumberFormatException
  {
    final Calendar c = Calendar.getInstance();
    c.setTime( date );
    return Format.sprintf( "%4d %02d %02d %02d", new Object[]
    {
        Integer.valueOf( Integer.toString( c.get( Calendar.YEAR ) ) ),
        Integer.valueOf( Integer.toString( c.get( Calendar.MONTH ) + 1 ) ),
        Integer.valueOf( Integer.toString( c.get( Calendar.DAY_OF_MONTH ) ) ),
        Integer.valueOf( Integer.toString( c.get( Calendar.HOUR_OF_DAY ) ) ) } );
  }

}
