package org.kalypso.services.test;

import java.util.Properties;

import javax.xml.rpc.ServiceException;
import javax.xml.rpc.Stub;

import junit.framework.TestCase;

import org.kalypso.services.ProxyFactory;

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
    m_conf.setProperty( ProxyFactory.KALYPSO_SERVER_URLS, "http://pc242:8080" );
    m_conf.setProperty( ProxyFactory.KALYPSO_PROXY_BASE, "org.kalypso.services.proxy" );
  }

  public void testGetProxy() throws ServiceException
  {
    ProxyFactory pf = new ProxyFactory( m_conf );
    
    final Stub proxy = pf.getProxy( "Kalypso_ObservationService", "IObservationService" );
    
    //pf.getProxy( "", "" );
  }
}
