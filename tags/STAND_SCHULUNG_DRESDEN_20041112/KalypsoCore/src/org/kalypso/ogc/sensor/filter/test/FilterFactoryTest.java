package org.kalypso.ogc.sensor.filter.test;

import java.io.InputStream;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.filter.FilterFactory;
import org.kalypso.ogc.sensor.timeseries.forecast.ForecastFilter;
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
  
  public void testCreateFilterWithString( ) throws SensorException
  {
    IObservation obs = FilterFactory.createFilter( "&lt;filter xmlns=&quot;filters.zml.kalypso.org&quot; xmlns:xlink=&quot;http://www.w3.org/1999/xlink&quot; xmlns:vc=&quot;valuecomp.filters.zml.kalypso.org&quot;&gt;&lt;forecastFilter&gt;&lt;zmlFilter&gt;&lt;zml xlink:href=&quot;kalypso-ocs:psicompact://HN.1_ES.02PG...501020.Gemessene?<from>2004-01-01T00:00:00</from><to>2004-01-04T12:45:32&</to>&quot;/&gt;&lt;/zmlFilter&gt;&lt;zmlFilter&gt;&lt;zml xlink:href=&quot;kalypso-ocs:psicompact://HN.1_ES.02PG...501020.Vorhergesagte?<from>2004-01-04T00:00:00</from><to>2004-01-06T22:30:00</to>&quot;/&gt;&lt;/zmlFilter&gt;&lt;/forecastFilter&gt;&lt;/filter&gt;" );
    assertTrue( obs instanceof ForecastFilter );
  }
}
