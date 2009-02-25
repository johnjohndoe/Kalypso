/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
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
package org.kalypso.ogc.gml.serialize.test;

import java.net.URL;

import org.junit.Test;
import org.kalypso.commons.performance.TimeLogger;
import org.kalypso.ogc.gml.serialize.GmlSerializer;

/**
 * @author Gernot Belger
 */
public class GmlSerializerTest
{
  @Test
  public void testLoadPerformance( ) throws Exception
  {
    final URL zipResource = getClass().getResource( "resources/grandeGml.zip" );
    final String externalForm = zipResource.toExternalForm();
    final URL gmlUrl = new URL( "jar:" + externalForm + "!/discretisation.gml" );

    final TimeLogger timeLogger = new TimeLogger( "GmlSerializer-Test" );
    timeLogger.printCurrentInterim( "Step 1: start: " );
    GmlSerializer.createGMLWorkspace( gmlUrl, null );
    timeLogger.takeInterimTime();
    timeLogger.printCurrentInterim( "Step 1: finished: " );
    timeLogger.printCurrentInterim( "Step 2: start_ " );
    GmlSerializer.createGMLWorkspace( gmlUrl, null );
    timeLogger.takeInterimTime();
    timeLogger.printCurrentInterim( "Step 2: finished: " );
  }

}

/**
 * Test history<br>
 * <br>
 * Before optimisation:<br>
 * - Step 1: 1m10s<br>
 * - Step 2: 1m4s<br>
 * <br>
 * <br>
 * No modified check at all:<br>
 * - Step 1: 38s<br>
 * - Step 2: 30s<br>
 * <br>
 */
