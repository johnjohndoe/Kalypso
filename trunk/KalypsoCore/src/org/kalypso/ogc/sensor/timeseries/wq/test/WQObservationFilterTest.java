package org.kalypso.ogc.sensor.timeseries.wq.test;

import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;

import junit.framework.TestCase;

import org.apache.commons.io.IOUtils;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.xml.sax.InputSource;

/**
 * WQObservationFilterTest
 * 
 * @author schlienger
 */
public class WQObservationFilterTest extends TestCase
{
  /**
   * first test
   * 
   * @throws MalformedURLException
   * @throws SensorException
   */
  public void testGetValues( ) throws MalformedURLException, SensorException
  {
    InputStream ins = null;
    try
    {
      ins = WQObservationFilterTest.class.getResourceAsStream( "wq-test.zml" );

      final IObservation obs = ZmlFactory
          .parseXML(
              new InputSource( ins ),
              "",
              new URL(
                  "file:/wq-test.zml?<filter><wqFilter xmlns=\"filters.zml.kalypso.org\" type=\"W\"/></filter>" ) );

      final ITuppleModel wqValues = obs.getValues( null );

      assertNotNull( wqValues );

      System.out.println( ObservationUtilities.dump( wqValues, "  " ) );
    }
    finally
    {
      IOUtils.closeQuietly( ins );
    }
  }

  /**
   * tests schirgiswalde
   * 
   * @throws MalformedURLException
   * @throws SensorException
   */
  public void testSchirgiswalde( ) throws MalformedURLException,
      SensorException
  {
    InputStream ins = null;
    try
    {
      ins = WQObservationFilterTest.class.getResourceAsStream( "wq-test2.zml" );

      final IObservation obs = ZmlFactory
          .parseXML(
              new InputSource( ins ),
              "",
              new URL(
                  "file:/wq-test2.zml?<filter><wqFilter xmlns=\"filters.zml.kalypso.org\" type=\"W\"/></filter>" ) );

      final ITuppleModel values = obs.getValues( null );

      assertNotNull( values );

      System.out.println( ObservationUtilities.dump( values, "  " ) );
    }
    finally
    {
      IOUtils.closeQuietly( ins );
    }
  }

  /**
   * tests a wq filter over an observation that does not have any wq param spec
   * 
   * @throws MalformedURLException
   * @throws SensorException
   */
  public void testObsWithoutWQParam( ) throws MalformedURLException,
      SensorException
  {
    InputStream ins = null;
    try
    {
      ins = WQObservationFilterTest.class.getResourceAsStream( "wq-test3.zml" );

      final IObservation obs = ZmlFactory
          .parseXML(
              new InputSource( ins ),
              "",
              new URL(
                  "file:/wq-test3.zml?<filter><wqFilter xmlns=\"filters.zml.kalypso.org\" type=\"W\"/></filter>" ) );

      final ITuppleModel values = obs.getValues( null );

      assertNotNull( values );

      System.out.println( ObservationUtilities.dump( values, "  " ) );
    }
    finally
    {
      IOUtils.closeQuietly( ins );
    }
  }
}