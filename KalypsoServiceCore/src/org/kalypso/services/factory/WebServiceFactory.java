package org.kalypso.services.factory;

import java.net.MalformedURLException;
import java.net.URL;
import java.rmi.Remote;

import javax.xml.namespace.QName;
import javax.xml.rpc.Service;
import javax.xml.rpc.ServiceException;

import org.kalypso.java.lang.reflect.ClassUtilities;

/**
 * Factory for Kalypso WebServices.
 * 
 * @author belger, schlienger
 */
public class WebServiceFactory
{
  /** the Kalypso Service Namespace */
  public static String SERVICE_NAME_SPACE_URI = "urn:Kalypso";


  private WebServiceFactory()
  {
  // not to be instanciated
  }

  /**
   * Creates a <code>Service</code>
   * 
   * @param baseURL the base URL of the server
   * @param serviceName the name of the service
   * @param intfClass the class of the service interface
   * 
   * @return Service
   * 
   * @throws MalformedURLException
   * @throws ServiceException
   */
  public static Service createServiceFactory( final String baseURL, final String serviceName, final Class intfClass )
      throws ServiceException, MalformedURLException
  {
    final String shortName = ClassUtilities.getOnlyClassName( intfClass );
  
    final String strUrl = baseURL.toString() + "/" + serviceName + "/" + shortName + "?WSDL";
    final URL wsdlUrl = new URL( strUrl );
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
    final String portName = ClassUtilities.getOnlyClassName( intfClass ) + "Port";

    return new QName( SERVICE_NAME_SPACE_URI, portName );
  }

  /**
   * Convenience method: calls <code>service.getPort( toPortName( intfClass ), intfClass )</code>.
   */
  public static Remote getPort( final Service service, final Class intfClass ) throws ServiceException
  {
    return service.getPort( toPortName( intfClass ), intfClass );
  }
}
