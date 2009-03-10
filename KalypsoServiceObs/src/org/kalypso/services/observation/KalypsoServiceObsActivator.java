package org.kalypso.services.observation;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Dictionary;
import java.util.Hashtable;

import javax.xml.namespace.QName;
import javax.xml.ws.Service;

import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.kalypso.ogc.sensor.zml.ZmlURLConstants;
import org.kalypso.services.observation.client.OcsURLStreamHandler;
import org.kalypso.services.observation.sei.IObservationService;
import org.kalypso.services.observation.server.ObservationServiceImpl;
import org.osgi.framework.BundleContext;
import org.osgi.service.url.URLConstants;
import org.osgi.service.url.URLStreamHandlerService;

/**
 * The main plugin class to be used in the desktop.
 */
public class KalypsoServiceObsActivator extends AbstractUIPlugin
{
  public final static String SYSPROP_CONFIGURATION_LOCATION = "kalypso.hwv.observation.service.configuration.location";

  public final static String SYSPROP_REINIT_SERVICE = "kalypso.hwv.observation.service.reinit.interval";

  // The shared instance.
  private static KalypsoServiceObsActivator plugin;

  private IObservationService m_observationService = null;

  /**
   * The constructor.
   */
  public KalypsoServiceObsActivator( )
  {
    plugin = this;
  }

  /**
   * This method is called upon plug-in activation
   */
  @Override
  public void start( final BundleContext context ) throws Exception
  {
    super.start( context );
  }

  /**
   * Registers the OCS-StreamURLHAndler iun order to support the 'kalypso-ocs' protocoll.<br>
   * Should be called from the application this stuff is running in, else it is not enforced that the protocol is registered before its first use.<br>
   * Maybe we should introduce some kind of 'protocol' extension point that is triggered in KalypsoCore or similar?
   */
  public static void registerOCSUrlHandler( final BundleContext context )
  {
    // register the observation webservice url stream handler
    final OcsURLStreamHandler handler = new OcsURLStreamHandler();

    final Dictionary<Object, Object> properties = new Hashtable<Object, Object>( 1 );
    properties.put( URLConstants.URL_HANDLER_PROTOCOL, new String[] { ZmlURLConstants.SCHEME_OCS } );
    context.registerService( URLStreamHandlerService.class.getName(), handler, properties );
  }

  /**
   * This method is called when the plug-in is stopped
   */
  @Override
  public void stop( final BundleContext context ) throws Exception
  {
    super.stop( context );
    plugin = null;
  }

  /**
   * Returns the shared instance.
   */
  public static KalypsoServiceObsActivator getDefault( )
  {
    return plugin;
  }

  /**
   * Convenience method that returns the observation service proxy.
   * 
   * @return WebService proxy for the IObservationService.
   */
  public synchronized IObservationService getObservationServiceProxy( )
  {
    if( m_observationService == null )
    {
      final Thread currentThread = Thread.currentThread();
      final ClassLoader contextClassLoader = currentThread.getContextClassLoader();
      try
      {
        // REMARK: see JaxbUtilities for an explanation.
        // Applies to JAX-WS instead of JAXB in this case (same mechanism).
        if( contextClassLoader.getClass().getName().equals( "org.eclipse.ant.internal.core.AntClassLoader" )  )
          currentThread.setContextClassLoader( KalypsoServiceObsActivator.class.getClassLoader() );

        final String namespaceURI = "http://server.observation.services.kalypso.org/";
        final String serviceImplName = ObservationServiceImpl.class.getSimpleName();

        final String wsdlLocationProperty = System.getProperty( "kalypso.hwv.observation.service.client.wsdl.location" );
        final URL wsdlLocation = new URL( wsdlLocationProperty );
        final QName serviceName = new QName( namespaceURI, serviceImplName + "Service" );
        final Service service = Service.create( wsdlLocation, serviceName );

        m_observationService = service.getPort( new QName( namespaceURI, serviceImplName + "Port" ), IObservationService.class );
      }
      catch( final MalformedURLException e )
      {
        e.printStackTrace();
        return null;
      }
      finally
      {
        // Always restore old context class loader, who knows what happens else...
        currentThread.setContextClassLoader( contextClassLoader );
      }
    }

    return m_observationService;
  }

  public static String getID( )
  {
    return getDefault().getBundle().getSymbolicName();
  }
}