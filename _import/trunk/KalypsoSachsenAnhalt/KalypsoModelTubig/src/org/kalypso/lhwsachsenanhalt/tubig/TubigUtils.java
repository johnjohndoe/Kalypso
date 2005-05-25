package org.kalypso.lhwsachsenanhalt.tubig;

import java.io.File;
import java.util.Calendar;
import java.util.GregorianCalendar;

import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;

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
/**
 * 
 * @author Thül
 *  
 */
public class TubigUtils
{
  /**
   * Gibt Datei-Extension zurück (z.B. png)
   *  
   */
  public static String getExtension( final File file )
  {
    String ext;
    String s;
    int pos;

    ext = null;
    s = file.getName();
    pos = s.lastIndexOf( '.' );

    if( pos > 0 && pos < s.length() - 1 )
    {
      ext = s.substring( pos + 1 );
    }
    return ext;
  }

  /**
   * gibt nur den Dateinamen zurück (ohne Dateiendung) <br>
   * wird verwendet, um den Kurznamen aus den Datei-Namen der TUBIG-Dateien zu
   * generieren
   */
  public static String getFileNameWOExt( final File file )
  {
    String sFleName;
    final String sExt;
    int pos;

    sFleName = file.getName();
    sExt = getExtension( file );
    pos = sFleName.lastIndexOf( "." + sExt );
    sFleName = sFleName.substring( 0, pos );
    return sFleName;
  }

  /**
   * gibt entsprechend der Datei-Extension die zugehörige TimeserieConstant für
   * ZML-Achse zurück
   * 
   * @author Thül
   */
  public static String getObservationType( final File fileTubig )
  {
    String sExt;
    String sObsType;

    sObsType = null;

    sExt = getExtension( fileTubig ).toUpperCase();
    if( "VW".equals( sExt ) || "VWS".equals( sExt ) )
      sObsType = TimeserieConstants.TYPE_WATERLEVEL;
    else if( "VNS".equals( sExt ) || "NS".equals( sExt ) || "PNS".equals( sExt ) )
      sObsType = TimeserieConstants.TYPE_RAINFALL;
    else if( "VVS".equals( sExt ) || "PV".equals( sExt ) || "VV".equals( sExt ) )
      sObsType = TimeserieConstants.TYPE_VOLUME;
    else if( "VSA".equals( sExt ) || "PSA".equals( sExt ) || "Q".equals( sExt )
        || "PQ".equals( sExt ) || "VQ".equals( sExt ) || "ZFL".equals( sExt ) )
      sObsType = TimeserieConstants.TYPE_RUNOFF;
    else
    {
      // Fehler werfen?
    }
    return sObsType;
  }

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
        + String.valueOf( gregCal.get( Calendar.MONTH ) + 1 ) + "."
        + String.valueOf( gregCal.get( Calendar.YEAR ) ) + " "
        + String.valueOf( gregCal.get( Calendar.HOUR_OF_DAY ) ) + ":"
        + String.valueOf( gregCal.get( Calendar.MINUTE ) ) + ":"
        + String.valueOf( gregCal.get( Calendar.SECOND ) );

    return sUhrzeit;
  }

}

