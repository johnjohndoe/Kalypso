package org.kalypso.ogc.sensor.timeseries.wq.test;

import junit.framework.TestCase;

import org.kalypso.ogc.sensor.timeseries.wq.WechmannException;
import org.kalypso.ogc.sensor.timeseries.wq.WechmannParams;

/**
 * @author schlienger
 */
public class WechmannParamsTest extends TestCase
{
  public void testWechmannParams() throws WechmannException
  {
    final WechmannParams wp1 = new WechmannParams( -38.12, -7.87274, 2.25925, 170 );
    
    System.out.println( wp1 );
    
    final WechmannParams wp2 = new WechmannParams( -43.32, -7.24065, 2.131 );
    
    System.out.println( wp2 );
    
    assertTrue( Double.compare( wp2.getWGR(), Double.MAX_VALUE ) == 0 );
  }
}
