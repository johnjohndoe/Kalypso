package org.kalypso.ogc.sensor.filter.test;

import java.io.InputStream;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.filter.FilterFactory;
import org.kalypso.ogc.sensor.timeseries.wq.WQObservationFilter;

import junit.framework.TestCase;

/**
 * FilterFactoryTest
 * 
 * @author schlienger
 */
public class FilterFactoryTest extends TestCase
{
  private InputStream m_ins;

  protected void setUp( ) throws Exception
  {
    super.setUp();
    
    m_ins = getClass().getResourceAsStream( "wqfilter.xml" );
  }
  
  protected void tearDown( ) throws Exception
  {
    super.tearDown();
    
    m_ins.close();
  }

  public void testCreateFilter( ) throws SensorException
  {
    IObservation obs = FilterFactory.createFilter( m_ins );
    
    assertTrue( obs instanceof WQObservationFilter );
  }
}
