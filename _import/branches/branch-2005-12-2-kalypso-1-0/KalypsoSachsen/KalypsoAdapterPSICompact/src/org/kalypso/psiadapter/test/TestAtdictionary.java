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

package org.kalypso.psiadapter.test;

import java.net.URL;

import junit.framework.TestCase;

import org.kalypso.ogc.sensor.timeseries.wq.wqtable.WQTable;
import org.kalypso.ogc.sensor.timeseries.wq.wqtable.WQTableSet;
import org.kalypso.psiadapter.PSICompactFactory;

/**
 * Test getting a WQTableSet for a fixed PSI-ID
 * 
 * @author Gernot Belger
 */
public class TestAtdictionary extends TestCase
{
  public void testFetchWQTableSet()
  {
    final URL atDictLocation = getClass().getResource( "resources/at.ini" );
    System.setProperty( PSICompactFactory.SYSPROP_AT_DICTIONARY, atDictLocation.toExternalForm() );

    final WQTableSet tableSet = PSICompactFactory.getWQTable( "ID1" );
    assertNotNull( tableSet );

    final WQTable[] tables = tableSet.getTables();
    assertEquals( 2, tables.length );

    final String firstDate = PSICompactFactory.DF_AT_DICT.format( tables[0].getValidity() );
    final String secondDate = PSICompactFactory.DF_AT_DICT.format( tables[1].getValidity() );

    assertEquals( "24.10.1972", firstDate );
    assertEquals( "02.06.2008", secondDate );

    final WQTableSet tableSet2 = PSICompactFactory.getWQTable( "ID1" );
    assertSame( tableSet, tableSet2 ); // should be same, as tables get cached

    // several ids in same group
    final WQTableSet tableSetId2 = PSICompactFactory.getWQTable( "ID2" );
    final WQTableSet tableSetId3 = PSICompactFactory.getWQTable( "ID3" );
    assertNotNull( tableSetId2 );
    assertNotNull( tableSetId3 );

    final WQTable[] tablesId2 = tableSetId2.getTables();
    assertEquals( 1, tablesId2.length );
    final WQTable[] tablesId3 = tableSetId3.getTables();
    assertEquals( 1, tablesId3.length );

    final String dateId2 = PSICompactFactory.DF_AT_DICT.format( tablesId2[0].getValidity() );
    assertEquals( "03.06.2008", dateId2 );
    final String dateId3 = PSICompactFactory.DF_AT_DICT.format( tablesId3[0].getValidity() );
    assertEquals( "03.06.2008", dateId3 );
  }

}
