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

package org.kalypso.ogc.sensor.timeseries.envelope;

import java.io.StringWriter;

import junit.framework.TestCase;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;

/**
 * @author schlienger
 */
public class TranProLinFilterTest extends TestCase
{
  public void testGetValues() throws SensorException
  {
    final IObservation obs = TimeserieUtils.createTestTimeserie( new String[]
    { "W" }, 10, false );

    assertEquals( 10, obs.getValues(null).getCount() );
    
    System.out.println( ObservationUtilities.dump( obs.getValues(null), "   " ) );
    
    final TranProLinFilter filter = new TranProLinFilter( null, null,"*", 1, 1.15 ,0,null);
    filter.initFilter( null, obs, null );
    
    assertEquals( 10, filter.getValues(null).getCount() );

    Number valueOrg = (Number)obs.getValues(null).getElement( 0, ObservationUtilities.findAxisByType( obs.getAxisList(), "W" ) );
    Number valueNew = (Number)filter.getValues(null).getElement( 0, ObservationUtilities.findAxisByType( filter.getAxisList(), "W" ) );
    assertEquals( valueOrg.doubleValue(), valueNew.doubleValue(), 0.001 );
    
    valueOrg = (Number)obs.getValues(null).getElement( 9, ObservationUtilities.findAxisByType( obs.getAxisList(), "W" ) );
    valueNew = (Number)filter.getValues(null).getElement( 9, ObservationUtilities.findAxisByType( filter.getAxisList(), "W" ) );
    assertEquals( valueOrg.doubleValue() * 1.15, valueNew.doubleValue(), 0.001 );

    final StringWriter w1 = new StringWriter();
    ObservationUtilities.dump( obs.getValues(null), "\t", w1 );

    final StringWriter w2 = new StringWriter();
    ObservationUtilities.dump( filter.getValues(null), "\t", w2 );
    
    assertFalse( w1.toString().equals( w2.toString() ) );
  }
}
