package org.kalypso.services.sensor.impl.test;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.rmi.RemoteException;
import java.util.Map;

import junit.framework.TestCase;

import org.kalypso.ogc.sensor.beans.ObservationBean;
import org.kalypso.ogc.sensor.beans.ObservationDataDescriptorBean;
import org.kalypso.repository.beans.ItemBean;
import org.kalypso.services.common.ServiceConfig;
import org.kalypso.services.sensor.impl.KalypsoObservationService;

/**
 * @author schlienger
 */
public class KalypsoObservationServiceTest extends TestCase
{
  private KalypsoObservationService m_srv;

  /**
   * @see junit.framework.TestCase#setUp()
   */
  protected void setUp() throws Exception
  {
    super.setUp();
    
    // because we are not in the server context
    System.setProperty( ServiceConfig.TEMP_DIR, System.getProperty( "java.io.tmpdir" ) );
    System.setProperty( ServiceConfig.CONF_DIR, getClass().getResource(".").getFile() );
    
    m_srv = new KalypsoObservationService();
  }
  
  public void testGetServiceVersion()
  {
    final int ver = m_srv.getServiceVersion();

    System.out.println( "Service version: " + ver );

    assertTrue( ver >= 0 );
  }

  public void testGetChildren() throws RemoteException
  {
    System.out.println( "Start Tree Listing:" );
    
    final ItemBean[] beans = m_srv.getChildren( null );

    for( int i = 0; i < beans.length; i++ )
      outputBean( beans[i], "#" );
    
    System.out.println( ":Stop Tree Listing" );
  }
  
  private void outputBean( final ItemBean bean, final String space ) throws RemoteException
  {
    assertNotNull( bean );
    
    System.out.println( space + bean.getId() + " - " + bean.getName() );
    
    if( m_srv.hasChildren( bean ) )
    {
      final ItemBean[] children = m_srv.getChildren( bean );
      
      for( int j = 0; j < children.length; j++ )
        outputBean( children[j], space + " " );
    }
  }
  
  public void testReadData() throws RemoteException, MalformedURLException
  {
    System.out.println( "Start Test Read Data:" );
    
    final ItemBean[] beans = m_srv.getChildren( null );

    for( int i = 0; i < beans.length; i++ )
      readData( beans[i], "#" );
    
    System.out.println( ":Stop Test Read Data" );
  }
  
  private void readData( final ItemBean bean, final String space ) throws RemoteException, MalformedURLException
  {
    if( bean instanceof ObservationBean )
    {
      final ObservationBean ob = (ObservationBean)bean;
      
      final Map map = ob.getMetadataList();
      
      System.out.println( space + "Metadata for " + ob.getName() + " are:" + map );
      
      final ObservationDataDescriptorBean oddb = m_srv.readData( ob );
      final URL url = new URL( oddb.getLocation() );
      
      System.out.println( space + "Data location: " + url + " [" + oddb.getFormat() + "]" );
      final File f = new File( url.getFile() );
      assertTrue( f.exists() );
      
      m_srv.clearTempData( oddb );
      
      System.out.println( space + "Data cleared" );
      assertFalse( f.exists() );
    }
    
    if( m_srv.hasChildren( bean ) )
    {
      final ItemBean[] children = m_srv.getChildren( bean );
      
      for( int j = 0; j < children.length; j++ )
        readData( children[j], space + " " );
    }
  }
}
