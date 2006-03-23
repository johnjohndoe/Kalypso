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
package org.kalypso.dwd;

import java.util.Date;
import java.util.Vector;

/**
 * @author doemming
 *         <p>
 *         Raster DWD Reader
 * 
 * 
 * Aufbau Kennsatz :<br>
 * 
 * Spalte 1- 11: Datum der Daten YYMMDDHHhh (1X,I10) 12-15 : Elementkennung der Daten (i4) hier :<br>
 * 
 * <li>114 : geogr. Breite in Grad*100000 des Gittermittelpunktes <br>
 * </li>
 * <li>115 : geogr. Laenge in Grad*100000 des Gittermittelpunktes <br>
 * </li>
 * <li>8 : geogr. Hoehe in m des Gittermittelpunktes</li>
 * 
 * <li>424 : Niederschlag</li>
 * <li>425 : Schnee</li>
 * <li>11 : Temperatur</li>
 * 
 * <p>
 * Die Werte der im Kennsatz definierten Elemente an den Gitterpunkten in der angegebenen Dimension. Die Gitterwerte
 * sind hintereinander zeilenweise von West nach Ost und von Sued nach Nord angeordnet .
 * 
 * Jeweils 13 Werte pro Zeile (13i8):
 * 
 * Spalte 1- 8 : Gitterwert 1 z.B. 4908634 bedeutet 49.08634 Grad geogr. Breite 9-16 : Gitterwert 2 4908878 49.08878
 * Aufbau_LMFiles.txt
 */
public class DWDRaster
{
  /**
   * Temperatur in 2m Hoehe [GradKelvin*10]
   */
  public static final int KEY_TEMP = 11;

  /** Taupunkt in 2m Hoehe [GradC*10] (lt. HerrSchmitt, DWD) */
  public static final int KEY_TAU = 17;

  /** zonaler Wind (Modellgitter) [m/s*100] (lt. HerrSchmitt, DWD) */
  public static final int KEY_WINDZ = 33;

  /** meridionaler Wind (Modellgitter) [m/s*100] (lt. HerrSchmitt, DWD) */
  public static final int KEY_WINDM = 34;

  /** Gesamtbedeckungsgrad [%] (lt. HerrSchmitt, DWD) */
  public static final int KEY_BEDECKUNG = 71;

  /** fluessiger Niederschlag gesamt [mm*100] (lt. HerrSchmitt, DWD) */
  public static final int KEY_RAIN = 424;

  /** fester Niederschlag gesamt [mm*100] (lt. HerrSchmitt, DWD) */
  public static final int KEY_SNOW = 425;

  public static final int KEY_HEIGHT = 8;

  public static final int KEY_100000_LAT = 114;

  public static final int KEY_100000_LON = 115;

  final int m_key;

  final Date m_date;

  final Vector m_data;

  public DWDRaster( final Date date, final int key )
  {
    m_key = key;
    m_date = date;
    m_data = new Vector();
  }

  public int getKey()
  {
    return m_key;
  }

  public void addValue( double value )
  {
    m_data.add( new Double( value ) );
  }

  public int size()
  {
    return m_data.size();
  }

  //  public double getElementAt( int index )
  //  {
  //    return m_data.elementAt( index );
  //  }

  public double getValueAt( int index )
  {
    return ( (Double)m_data.elementAt( index ) ).doubleValue();
  }

  public Date getDate()
  {
    return m_date;
  }

  //  public double getSum( int ids[] )
  //  {
  //    double result = 0;
  //    for( int i = 0; i < ids.length; i++ )
  //      result += getValueAt( ids[i] );
  //    return result;
  //  }

  //  public double getAverage( int ids[] )
  //  {
  //    if( ids.length == 0 )
  //      return 0;
  //    double result = 0;
  //    for( int i = 0; i < ids.length; i++ )
  //      result += getValueAt( ids[i] );
  //    return result / ids.length;
  //  }

  //  public double getMin()
  //  {
  //    boolean found = false;
  //    double result = 0;
  //    for( int i = 0, j = size(); i < j; i++ )
  //    {
  //      double value = getValueAt( i );
  //      if( !found )
  //      {
  //        result = value;
  //        found = true;
  //      }
  //      else
  //      {
  //        if( value < result )
  //          result = value;
  //      }
  //    }
  //    return result;
  //  }
  //
  //  public double getMax()
  //  {
  //    boolean found = false;
  //    double result = 0;
  //    for( int i = 0, j = size(); i < j; i++ )
  //    {
  //      double value = getValueAt( i );
  //      if( !found )
  //      {
  //        result = value;
  //        found = true;
  //      }
  //      else
  //      {
  //        if( value > result )
  //          result = value;
  //      }
  //    }
  //    return result;
  //  }
}
