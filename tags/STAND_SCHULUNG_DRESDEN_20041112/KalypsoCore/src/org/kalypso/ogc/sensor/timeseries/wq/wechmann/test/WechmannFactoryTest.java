package org.kalypso.ogc.sensor.timeseries.wq.wechmann.test;

import java.io.StringReader;
import java.util.Date;

import javax.xml.bind.JAXBException;

import junit.framework.TestCase;

import org.kalypso.ogc.sensor.timeseries.wq.wechmann.WechmannException;
import org.kalypso.ogc.sensor.timeseries.wq.wechmann.WechmannFactory;
import org.kalypso.ogc.sensor.timeseries.wq.wechmann.WechmannGroup;
import org.kalypso.ogc.sensor.timeseries.wq.wechmann.WechmannParams;
import org.kalypso.ogc.sensor.timeseries.wq.wechmann.WechmannSet;
import org.xml.sax.InputSource;

/**
 * WechmannFactoryTest
 * 
 * @author schlienger
 */
public class WechmannFactoryTest extends TestCase
{
  private WechmannGroup m_wg;

  /**
   * @see junit.framework.TestCase#setUp()
   */
  protected void setUp() throws Exception
  {
    WechmannParams wp1 = new WechmannParams( -38.12, -7.87274, 2.25925, 170 );
    WechmannParams wp2 = new WechmannParams( -43.32, -7.24065, 2.131 );
    WechmannSet ws = new WechmannSet( new Date(), new WechmannParams[] {
        wp1, wp2 } );

    m_wg = new WechmannGroup( new WechmannSet[] { ws, ws } );
  }

  public void testParse() throws WechmannException, JAXBException
  {
    final String xml = WechmannFactory.createXMLString( m_wg );
    
    assertNotNull( xml );
    assertTrue( xml.length() > 0 );
    
    System.out.println( xml );
    
    final WechmannGroup group = WechmannFactory.parse( new InputSource( new StringReader( xml )) );
    
    assertNotNull( group );
  }
}