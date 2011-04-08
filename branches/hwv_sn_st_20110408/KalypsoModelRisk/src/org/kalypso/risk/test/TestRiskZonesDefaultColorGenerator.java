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
package org.kalypso.risk.test;

import java.awt.Color;

/**
 * @author Dejan Antanaskovic
 * 
 */
public class TestRiskZonesDefaultColorGenerator
{
  /**
   * This class defines default risk zones colors, specified by LSBG. <br>
   * <br>
   * <table>
   * <tr>
   * <th>Art der Fläche</th>
   * <th>Art der Darstellung</th>
   * <th>Farbe</th>
   * <th>HSV Farbmodell</th>
   * </tr>
   * <tr>
   * <td>mäßige Betroffenheit, bebaute Flächen</td>
   * <td>Flächenfüllung</td>
   * <td>gelb - Solar Yellow</td>
   * <td>(60, 1.0, 1.0)</td>
   * </tr>
   * <tr>
   * <td>mittlere Betroffenheit, bebaute Flächen</td>
   * <td>Flächenfüllung</td>
   * <td>orange - Electron Gold</td>
   * <td>(40, 1.0, 1.0)</td>
   * </tr>
   * <tr>
   * <td>hohe Betroffenheit, bebaute Flächen</td>
   * <td>Flächenfüllung</td>
   * <td>rot - Mars Red</td>
   * <td>(0, 1.0, 1.0)</td>
   * </tr>
   * <tr>
   * <td>mäßige Betroffenheit, Freiflächen</td>
   * <td>Flächenfüllung</td>
   * <td>pink - Fushia Pink</td>
   * <td>(314, 0.55, 1.0)</td>
   * </tr>
   * <tr>
   * <td>mittlere Betroffenheit, Freiflächen</td>
   * <td>Flächenfüllung</td>
   * <td>lila - Dark Amethyst</td>
   * <td>(287, 1.0, 0.66)</td>
   * </tr>
   * </table>
   */
  public static void main( final String[] args )
  {
    System.out.println( new Color( Color.HSBtoRGB( 60 / (float) 360, 1, 1 ) ) );
    System.out.println( new Color( Color.HSBtoRGB( 40 / (float) 360, 1, 1 ) ) );
    System.out.println( new Color( Color.HSBtoRGB( 0 / (float) 360, 1, 1 ) ) );
    System.out.println( new Color( Color.HSBtoRGB( 314 / (float) 360, (float) 0.55, 1 ) ) );
    System.out.println( new Color( Color.HSBtoRGB( 287 / (float) 360, 1, (float) 0.66 ) ) );
  }

}
