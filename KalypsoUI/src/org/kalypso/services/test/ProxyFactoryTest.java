package org.kalypso.services.test;

import java.util.Properties;

import javax.xml.rpc.ServiceException;
import javax.xml.rpc.Stub;

import junit.framework.TestCase;

import org.kalypso.services.ProxyFactory;
import org.kalypso.services.proxy.IObservationService;
import org.kalypso.services.proxy.Kalypso_ObservationService_Impl;

/**
 * @author schlienger
 */
public class ProxyFactoryTest extends TestCase
{
  private Properties m_conf;

  /**
   * @see junit.framework.TestCase#setUp()
   */
  protected void setUp() throws Exception
  {
    super.setUp();
    
    m_conf = new Properties();
    m_conf.setProperty( "Kalypso_ObservationService" + "_URL", "http://pc242:8080" );
    m_conf.setProperty( ProxyFactory.KALYPSO_PROXY_BASE, "org.kalypso.services.proxy" );
  }

  public void testGetProxy() throws ServiceException
  {
    ProxyFactory pf = new ProxyFactory( m_conf );
    
    final Stub proxy = pf.getProxy( "Kalypso_ObservationService", "IObservationService" );
    assertTrue( proxy instanceof IObservationService );
    
    final IObservationService port = new Kalypso_ObservationService_Impl().getIObservationServicePort();
    assertNotNull( port );
    
    //pf.getProxy( "", "" );
  }
}
