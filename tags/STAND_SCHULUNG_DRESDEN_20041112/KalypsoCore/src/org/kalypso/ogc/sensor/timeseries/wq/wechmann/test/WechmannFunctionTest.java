package org.kalypso.ogc.sensor.timeseries.wq.wechmann.test;

import org.kalypso.java.util.DoubleComparator;
import org.kalypso.ogc.sensor.timeseries.wq.wechmann.WechmannException;
import org.kalypso.ogc.sensor.timeseries.wq.wechmann.WechmannFunction;
import org.kalypso.ogc.sensor.timeseries.wq.wechmann.WechmannParams;

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
  
  public void testComputeQ() throws WechmannException
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

  public void testComputeW() throws WechmannException
  {
    assertTrue( m_dc1.compare( WechmannFunction.computeW( m_wp1, 5.82 ), 33 ) == 0 );
    assertTrue( m_dc1.compare( WechmannFunction.computeW( m_wp1, 27.8 ), 104 ) == 0 );
    
    assertTrue( m_dc1.compare( WechmannFunction.computeW( m_wp2, 67.8 ), 173 ) == 0 );
    assertTrue( m_dc1.compare( WechmannFunction.computeW( m_wp2, 144 ), 265 ) == 0 );
  }
  
  public void testForSchirgiswalde() 
  {
    WechmannParams wp1 = new WechmannParams( 99.96, -8.43382, 2.25920, 230 );
    /*WechmannParams wp2 = */new WechmannParams( 89.99, -7.49066, 2.03671 );

    assertEquals( 3.21, WechmannFunction.computeQ( wp1, 170 ), 0.01 );
    assertEquals( 4.59, WechmannFunction.computeQ( wp1, 182 ), 0.01 );
  }
}
