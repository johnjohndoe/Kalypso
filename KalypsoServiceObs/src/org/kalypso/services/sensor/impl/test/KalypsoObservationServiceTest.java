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
    final ObservationBean ob = m_srv.adaptItem( bean );
    if( ob != null )
    {
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
      System.out.println( space + "Bean not adaptable: " + bean.getName() + " ID=" + bean.getId() );
    }
    
    if( m_srv.hasChildren( bean ) )
    {
      final ItemBean[] children = m_srv.getChildren( bean );
      
      for( int j = 0; j < children.length; j++ )
        readData( children[j], space + " " );
    }
  }
  
  public void testFindItem() throws RemoteException, MalformedURLException
  {
    final ItemBean b1 = m_srv.findItem( "Spree://2004/PA_GROEDI.zml" );
    assertNotNull( b1 );
    assertFalse( m_srv.hasChildren( b1 ) );

    final ItemBean b2 = m_srv.findItem( "psicompact://PSI-ROOT.LEVEL_1.SUBLEVEL_1.PEGEL_111.m" );
    assertNotNull( b2 );
    assertFalse( m_srv.hasChildren( b2 ) );
    
    final ItemBean b3 = m_srv.findItem( "Spree://2004" );
    assertNotNull( b3 );
    assertTrue( m_srv.hasChildren( b3 ) );
    
    final ItemBean b4 = m_srv.findItem( "inexistent-id" );
    assertNull( b4 );
      
    final ItemBean b5 = m_srv.findItem( "foo://XYZ" );
    assertNull( b5 );
  }

  public void testWriteData() throws RemoteException, MalformedURLException
  {
    // real
    final String id = "Test://test.zml";
    final String rep = "Test";
    final ObservationBean ob1 = new ObservationBean( id, "test", rep, null );
    
    final String wFile = new File( KALYPSO_SERVER_BASE + "\\data\\tmp\\_test\\W_BAUTZWB.zml").toURL().toExternalForm();
    final OCSDataBean db1 = new OCSDataBean( 0, ob1.getId(), wFile );
    
    m_srv.writeData( ob1, db1 );
  }
}
