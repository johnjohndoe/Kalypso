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
package org.kalypso.ogc.sensor.timeseries.wq.wechmann.test;

import junit.framework.TestCase;

import org.kalypso.ogc.sensor.timeseries.wq.WQException;
import org.kalypso.ogc.sensor.timeseries.wq.wechmann.WechmannParams;

/**
 * @author schlienger
 */
public class WechmannParamsTest extends TestCase
{
  public void testWechmannParams() throws WQException
  {
    final WechmannParams wp1 = new WechmannParams( -38.12, -7.87274, 2.25925, 170 );

    final WechmannParams wp2 = new WechmannParams( -43.32, -7.24065, 2.131 );

    assertTrue( wp1.hasWGR() );
    assertTrue( Double.compare( wp2.getWGR(), Double.MAX_VALUE ) == 0 );
    assertFalse( wp2.hasWGR() );
  }
}
