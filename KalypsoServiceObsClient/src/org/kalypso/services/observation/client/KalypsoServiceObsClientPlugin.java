package org.kalypso.services.observation.client;

import java.net.URLStreamHandler;
import java.util.Dictionary;
import java.util.Hashtable;

import javax.xml.rpc.ServiceException;

import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.kalypso.core.client.KalypsoServiceCoreClientPlugin;
import org.kalypso.ogc.sensor.zml.ZmlURLConstants;
import org.kalypso.services.ocs.OcsURLStreamHandler;
import org.kalypso.services.sensor.impl.KalypsoObservationService;
import org.osgi.framework.BundleContext;
import org.osgi.service.url.URLConstants;
import org.osgi.service.url.URLStreamHandlerService;

/**
 * The main plugin class to be used in the desktop.
 */
public class KalypsoServiceObsClientPlugin extends AbstractUIPlugin
{

  // The shared instance.
  private static KalypsoServiceObsClientPlugin plugin;

  /**
   * The constructor.
   */
  public KalypsoServiceObsClientPlugin( )
  {
    plugin = this;
  }

  /**
   * This method is called upon plug-in activation
   */
  @Override
  public void start( BundleContext context ) throws Exception
  {
    super.start( context );
    
    configureURLStreamHandler( context );

  }
  
  /**
   * Eclipse comes with its own StreamHandler proxy. So we just need to say which Handler to use for the protocol we can
   * cover.
   * <p>
   * Following handlers are registered:
   * <ul>
   * <li>OcsURLStreamHandler for 'kalypso-ocs' protocol. Handles Observation WebService urls.</li>
   * <li>XXX: insert your own handlers here...</li>
   * </ul>
   */
  private void configureURLStreamHandler( final BundleContext context )
  {
    // register the observation webservice url stream handler
    registerUrlStreamHandler( context, ZmlURLConstants.SCHEME_OCS, new OcsURLStreamHandler() );
  }

  private void registerUrlStreamHandler( final BundleContext context, final String scheme, final URLStreamHandler handler )
  {
    final Dictionary<Object, Object> properties = new Hashtable<Object, Object>( 1 );
    properties.put( URLConstants.URL_HANDLER_PROTOCOL, new String[] { scheme } );
    context.registerService( URLStreamHandlerService.class.getName(), handler, properties );
  }

  /**
   * This method is called when the plug-in is stopped
   */
  @Override
  public void stop( BundleContext context ) throws Exception
  {
    super.stop( context );
    plugin = null;
  }

  /**
   * Returns the shared instance.
   */
  public static KalypsoServiceObsClientPlugin getDefault( )
  {
    return plugin;
  }

  /**
   * Convenience method that returns the observation service proxy
   * 
   * @return WebService proxy for the IObservationService
   * @throws ServiceException
   *           TODO: Gernot: I think it is nonsense to invoke the proxy via the serviceCoreClientPlugin via reflektion, because
   *           we do know, how to invike it direktly
   */
  public KalypsoObservationService getObservationServiceProxy( ) throws ServiceException
  {
    return (KalypsoObservationService) KalypsoServiceCoreClientPlugin.getDefault().getProxyFactory().getAnyProxy( "Kalypso_ObservationService", "IObservationService" );
  }

  public static String getID( )
  {
    return getDefault().getBundle().getSymbolicName();
  }

}
