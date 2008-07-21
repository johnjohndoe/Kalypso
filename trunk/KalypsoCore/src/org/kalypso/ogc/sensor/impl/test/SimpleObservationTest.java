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

package org.kalypso.ogc.sensor.impl.test;

import junit.framework.TestCase;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.impl.SimpleAxis;
import org.kalypso.ogc.sensor.impl.SimpleObservation;
import org.kalypso.ogc.sensor.status.KalypsoStatusUtils;
import org.kalypso.ogc.sensor.zml.ZmlFactory;

/**
 * @author schlienger
 */
public class SimpleObservationTest extends TestCase
{
  private IObservation m_obs;

  /**
   * @see junit.framework.TestCase#setUp()
   */
  @Override
  protected void setUp() throws Exception
  {
    m_obs = getTestObservation();
  }
  
  public void testSetValues() throws SensorException
  {
    final ITuppleModel values = m_obs.getValues(null);
    
    final SimpleObservation observation = new SimpleObservation( values.getAxisList() );
    observation.setValues( values );
  }
  
  public static IObservation getTestObservation() throws SensorException
  {
    return ZmlFactory.parseXML( SimpleObservationTest.class.getResource( "test.zml" ), "" ); //$NON-NLS-1$ //$NON-NLS-2$
  }
  
  public void testAxis()
  {
    final IAxis[] axes = m_obs.getAxisList();
    
    final SimpleAxis axis = new SimpleAxis( axes[0] );
    
    assertTrue( axes[0].equals( axis ) );
    assertFalse( axes[1].equals( axis ) );
    
    IAxis saxes1 = KalypsoStatusUtils.createStatusAxisFor( axes[0], false );
    IAxis saxes2 = KalypsoStatusUtils.createStatusAxisFor( axes[1], false );
    IAxis saxis = KalypsoStatusUtils.createStatusAxisFor( axis, false );
    
    assertTrue( saxes1.equals( saxis ) );

    assertFalse( saxes1.equals( saxes2 ) );
    assertFalse( saxis.equals( saxes2 ) );
  }
}
