package org.kalypso.services.sensor.impl.test;

import java.rmi.RemoteException;

import junit.framework.TestCase;

import org.kalypso.repository.beans.ItemBean;
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
}
