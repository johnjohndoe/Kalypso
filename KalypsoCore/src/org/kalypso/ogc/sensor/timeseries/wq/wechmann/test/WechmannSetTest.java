package org.kalypso.ogc.sensor.timeseries.wq.wechmann.test;

import java.util.Date;

import junit.framework.TestCase;

import org.kalypso.ogc.sensor.timeseries.wq.wechmann.WechmannFunction;
import org.kalypso.ogc.sensor.timeseries.wq.wechmann.WechmannParams;
import org.kalypso.ogc.sensor.timeseries.wq.wechmann.WechmannSet;

/**
 * @author schlienger
 */
public class WechmannSetTest extends TestCase
{
  private WechmannParams m_wp1;
  private WechmannParams m_wp2;
  private WechmannSet m_ws;

  protected void setUp() throws Exception
  {
    m_wp1 = new WechmannParams( -38.12, -7.87274, 2.25925, 170 );
    m_wp2 = new WechmannParams( -43.32, -7.24065, 2.131 );
    m_ws = new WechmannSet( new Date(), new WechmannParams[] { m_wp1, m_wp2 } );
  }

  public void testGetForW()
  {
    assertEquals( m_ws.getForW( 100 ), m_wp1 );
    assertEquals( m_ws.getForW( 170 ), m_wp1 );
    assertNotSame( m_ws.getForW( 137 ), m_wp2 );
    
    assertEquals( m_ws.getForW( 171 ), m_wp2 );
    assertEquals( m_ws.getForW( 200 ), m_wp2 );
    assertNotSame( m_ws.getForW( 188 ), m_wp1 );
  }

  public void testGetForQ() 
  {
    assertEquals( m_ws.getForQ( 20.5 ), m_wp1 );
    
    double d = WechmannFunction.computeQ( m_ws.getForW( 170 ), 170 );
    assertEquals( m_ws.getForQ( d ), m_wp1 );
    assertNotSame( m_ws.getForQ( 45.2 ), m_wp2 );
    
    assertEquals( m_ws.getForQ( 66.5 ), m_wp2 );
    assertEquals( m_ws.getForQ( 145 ), m_wp2 );
    assertNotSame( m_ws.getForQ( 96.6 ), m_wp1 );
  }
}
