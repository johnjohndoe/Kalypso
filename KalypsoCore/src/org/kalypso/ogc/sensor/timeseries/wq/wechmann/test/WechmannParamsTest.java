package org.kalypso.ogc.sensor.timeseries.wq.wechmann.test;

import junit.framework.TestCase;

import org.kalypso.ogc.sensor.timeseries.wq.wechmann.WechmannParams;

/**
 * @author schlienger
 */
public class WechmannParamsTest extends TestCase
{
  public void testWechmannParams() 
  {
    final WechmannParams wp1 = new WechmannParams( -38.12, -7.87274, 2.25925, 170 );
    
    final WechmannParams wp2 = new WechmannParams( -43.32, -7.24065, 2.131 );
    
    assertTrue( wp1.hasWGR() );
    assertTrue( Double.compare( wp2.getWGR(), Double.MAX_VALUE ) == 0 );
    assertFalse( wp2.hasWGR() );
  }
}
