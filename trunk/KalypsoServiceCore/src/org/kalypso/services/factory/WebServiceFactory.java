package org.kalypso.services.factory;

import java.net.MalformedURLException;
import java.net.URL;

import javax.xml.namespace.QName;
import javax.xml.rpc.Service;
import javax.xml.rpc.ServiceException;

/**
 * Factory for Kalypso WebServices.
 * 
 * @author belger, schlienger
 */
public class WebServiceFactory
{
  /** the Kalypso Service Namespace */
  public final static String SERVICE_NAME_SPACE_URI = "urn:Kalypso";


  private WebServiceFactory()
  {
  // not to be instanciated
  }

  /**
   * Creates a <code>Service</code>
   * 
   * @param baseURL
   * @param serviceName
   * @return Service
   * 
   * @throws MalformedURLException
   * @throws ServiceException
   */
  public static Service createServiceFactory( final String baseURL, final String serviceName )
      throws ServiceException, MalformedURLException
  {
    final URL wsdlUrl = new URL( baseURL.toString() + "/KalypsoServices/RandomServiceEndpoint?WSDL" );
    final javax.xml.rpc.ServiceFactory factory = javax.xml.rpc.ServiceFactory.newInstance();

    return factory.createService( wsdlUrl, new QName( SERVICE_NAME_SPACE_URI, serviceName ) );
  }

  /**
   * Creates a QName necessary for creating services, given the interface for
   * which the service should be created.
   * 
   * @param intfClass
   *          the Class of the interface
   */
  public static QName toPortName( final Class intfClass )
  {
    // just convert real interface name to the port name
    final String className = intfClass.getName().substring(
        intfClass.getName().lastIndexOf( '.' ) + 1 );

    return new QName( SERVICE_NAME_SPACE_URI, className + "Port" );
  }
}