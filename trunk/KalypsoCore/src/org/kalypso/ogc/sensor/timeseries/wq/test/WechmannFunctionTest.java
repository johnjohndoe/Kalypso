package org.kalypso.ogc.sensor.timeseries.wq.test;

import org.kalypso.java.util.DoubleComparator;
import org.kalypso.ogc.sensor.timeseries.wq.WechmannFunction;
import org.kalypso.ogc.sensor.timeseries.wq.WechmannParams;

import junit.framework.TestCase;

/**
 * @author schlienger
 */
public class WechmannFunctionTest extends TestCase
{
  private final static DoubleComparator m_dc01 = new DoubleComparator( 0.1 );
  private final static DoubleComparator m_dc1 = new DoubleComparator( 1 );
  
  private WechmannParams m_wp1;
  private WechmannParams m_wp2;

  /**
   * @see junit.framework.TestCase#setUp()
   */
  protected void setUp() throws Exception
  {
    m_wp1 = new WechmannParams( -38.12, -7.87274, 2.25925, 170 );
    m_wp2 = new WechmannParams( -43.32, -7.24065, 2.131 );
  }
  
  public void testComputeQ()
  {
    assertTrue( m_dc01.compare( WechmannFunction.computeQ( m_wp1, 115 ), 32.9 ) == 0 );
    assertTrue( m_dc01.compare( WechmannFunction.computeQ( m_wp1, 168 ), 64.4 ) == 0 );
    
    assertTrue( m_dc01.compare( WechmannFunction.computeQ( m_wp2, 170 ), 65.9 ) == 0 );
    assertTrue( m_dc1.compare( WechmannFunction.computeQ( m_wp2, 293 ), 174 ) == 0 );
    

    double q = WechmannFunction.computeQ( m_wp1, 115 );
    assertEquals( 115, WechmannFunction.computeW( m_wp1, q ), 0.0001 );
    
    q = WechmannFunction.computeQ( m_wp1, 168 );
    assertEquals( 168, WechmannFunction.computeW( m_wp1, q ), 0.0001 );
    
    q = WechmannFunction.computeQ( m_wp2, 170 );
    assertEquals( 170, WechmannFunction.computeW( m_wp2, q ), 0.0001 );
    
    q = WechmannFunction.computeQ( m_wp2, 293 );
    assertEquals( 293, WechmannFunction.computeW( m_wp2, q ), 0.0001 );
  }

  public void testComputeW()
  {
    assertTrue( m_dc1.compare( WechmannFunction.computeW( m_wp1, 5.82 ), 33 ) == 0 );
    assertTrue( m_dc1.compare( WechmannFunction.computeW( m_wp1, 27.8 ), 104 ) == 0 );
    
    assertTrue( m_dc1.compare( WechmannFunction.computeW( m_wp2, 67.8 ), 173 ) == 0 );
    assertTrue( m_dc1.compare( WechmannFunction.computeW( m_wp2, 144 ), 265 ) == 0 );
  }
}
