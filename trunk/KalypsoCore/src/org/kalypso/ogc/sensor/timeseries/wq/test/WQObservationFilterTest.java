package org.kalypso.ogc.sensor.timeseries.wq.test;

import java.io.InputStream;
import java.net.URL;

import org.apache.commons.io.IOUtils;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.timeseries.wq.WQObservationFilter;
import org.kalypso.ogc.sensor.timeseries.wq.WQTuppleModel;
import org.kalypso.ogc.sensor.timeseries.wq.wechmann.WechmannException;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.xml.sax.InputSource;

import junit.framework.TestCase;

/**
 * WQObservationFilterTest
 * 
 * @author schlienger
 */
public class WQObservationFilterTest extends TestCase
{
  private IObservation obs;

  /**
   * Note: this also tests the FilterFactory class.
   * 
   * @see junit.framework.TestCase#setUp()
   * @see org.kalypso.ogc.sensor.filter.FilterFactory
   */
  protected void setUp( ) throws Exception
  {
    InputStream ins = null;
    try
    {
      ins = WQObservationFilterTest.class.getResourceAsStream( "wq-test.zml" );

      obs = ZmlFactory.parseXML( new InputSource( ins ), "", new URL(
          "file:/wq-test.zml#filter(wq*W)" ) );

      assertTrue( obs instanceof WQObservationFilter );
    }
    finally
    {
      IOUtils.closeQuietly( ins );
    }
  }

  public void testGetValues( ) throws SensorException, WechmannException
  {
    final ITuppleModel wqValues = obs.getValues( null );

    assertNotNull( wqValues );
    assertTrue( wqValues instanceof WQTuppleModel );

    System.out.println( ObservationUtilities.dump( wqValues, "  " ) );
  }
}