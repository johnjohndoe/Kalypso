package org.kalypso.ogc.sensor.timeseries.test;

import java.io.InputStream;
import java.io.StringReader;

import org.apache.commons.io.IOUtils;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.timeseries.WQTuppleModel;
import org.kalypso.ogc.sensor.timeseries.wq.WechmannException;
import org.kalypso.ogc.sensor.timeseries.wq.WechmannFactory;
import org.kalypso.ogc.sensor.timeseries.wq.WechmannGroup;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.xml.sax.InputSource;

import junit.framework.TestCase;

/**
 * WQTuppleModelTest
 * 
 * @author schlienger
 */
public class WQTuppleModelTest extends TestCase
{
  private IObservation obs;

  /**
   * @see junit.framework.TestCase#setUp()
   */
  protected void setUp( ) throws Exception
  {
    InputStream ins = null;
    try
    {
      ins = WQTuppleModelTest.class.getResourceAsStream( "wq-test.zml" );

      obs = ZmlFactory.parseXML( new InputSource( ins ), "", null );
    }
    finally
    {
      IOUtils.closeQuietly( ins );
    }
  }

  public void testGetElement( ) throws SensorException, WechmannException
  {
    final ITuppleModel values = obs.getValues( null );

    final String wechmann = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>" + obs.getMetadataList().getProperty( TimeserieConstants.MD_WQ );
    final WechmannGroup group = WechmannFactory.parse( new InputSource( new StringReader( wechmann ) ) );
    
    final WQTuppleModel wqValues = new WQTuppleModel( values, TimeserieConstants.TYPE_WATERLEVEL, group );
    
    assertNotNull( wqValues );
    
    System.out.println( ObservationUtilities.dump( wqValues, "  " ) );
  }

//  public void testSetElement( )
//  {
//
//  }
}