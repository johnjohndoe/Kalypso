package org.kalypso.services.sensor.impl.test;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.rmi.RemoteException;
import java.util.Map;

import junit.framework.TestCase;

import org.kalypso.ogc.sensor.beans.DateRangeBean;
import org.kalypso.ogc.sensor.beans.OCSDataBean;
import org.kalypso.ogc.sensor.beans.ObservationBean;
import org.kalypso.repository.beans.ItemBean;
import org.kalypso.services.common.ServiceConfig;
import org.kalypso.services.sensor.impl.KalypsoObservationService;
import org.kalypso.util.runtime.args.DateRangeArgument;

/**
 * @author schlienger
 */
public class KalypsoObservationServiceTest extends TestCase
{
  private final static String KALYPSO_SERVER_BASE = "\\\\pc242\\KalypsoServer";
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
     
      System.out.println( space + "Bean is observation: " + ob.getId() );
      System.out.println( space + "Metadata for " + ob.getName() + " are:" + map );
      
      final DateRangeArgument dra = DateRangeArgument.createFromPastDays( 30 );
      final DateRangeBean drb = new DateRangeBean( dra.getFrom(), dra.getTo() );
      
      final OCSDataBean oddb = m_srv.readData( ob, drb );
      final URL url = new URL( oddb.getLocation() );
      
      System.out.println( space + "Data location: " + url );
      final File f = new File( url.getFile() );
      assertTrue( f.exists() );
      
      m_srv.clearTempData( oddb );
      
      System.out.println( space + "Data cleared" );
      assertFalse( f.exists() );
    }
    else
    {
      System.out.println( space + "Bean not an observation: " + bean.getName() + " ID=" + bean.getId() );
    }
    
    if( m_srv.hasChildren( bean ) )
    {
      final ItemBean[] children = m_srv.getChildren( bean );
      
      for( int j = 0; j < children.length; j++ )
        readData( children[j], space + " " );
    }
  }
  
  public void testFindItem() throws RemoteException
  {
    final ItemBean b1 = m_srv.findItem( KALYPSO_SERVER_BASE + "\\data\\mirrored\\SomeObservations\\NEU\\PA_GROEDI.zml" );
    
    assertNotNull( b1 );
    
    assertFalse( m_srv.hasChildren( b1 ) );
    
    assertTrue( b1 instanceof ObservationBean );
    
    
    final ItemBean b3 = m_srv.findItem( KALYPSO_SERVER_BASE + "\\data\\mirrored\\SomeObservations" );
    
    assertNotNull( b3 );
    
    assertTrue( m_srv.hasChildren( b3 ) );
    
    assertFalse( b3 instanceof ObservationBean );
    
    try
    {
      m_srv.findItem( "inexistent-id" );
      
      throw new IllegalStateException("Precedent call should throw exception");
    }
    catch( RemoteException e )
    {
      // ok if exception is thrown
    }

    try
    {
      m_srv.findItem( KALYPSO_SERVER_BASE + "\\XYZ" );
      
      throw new IllegalStateException("Precedent call should throw exception");
    }
    catch( RemoteException e )
    {
      // ok if exception is thrown
    }
  }

  public void testWriteData() throws RemoteException
  {
    // real
    final ObservationBean ob1 = new ObservationBean( KALYPSO_SERVER_BASE + "\\data\\mirrored\\SomeObservations\\test\\test.zml", "test", "file", null );
    final OCSDataBean db1 = new OCSDataBean( 0, ob1.getId(), "file:" + KALYPSO_SERVER_BASE + "\\data\\tmp\\test\\example.zml" );
    
    m_srv.writeData( ob1, db1 );

    // fake
    final ObservationBean ob2 = new ObservationBean( KALYPSO_SERVER_BASE + "\\data\\mirrored\\SomeObservations\\fake-fake-fake", "test", "file", null );
    final OCSDataBean db2 = new OCSDataBean( 0, ob2.getId(), KALYPSO_SERVER_BASE + "\\data\\tmp\\test\\example.zml" );
    
    try
    {
      m_srv.writeData( ob2, db2 );
      
      throw new IllegalStateException( "Precedent call should throw exception" );
    }
    catch( RemoteException e )
    {
      // ok, should throw exception
    }
  }
}
