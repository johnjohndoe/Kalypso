package org.kalypso.core.client;

import java.util.MissingResourceException;
import java.util.Properties;
import java.util.ResourceBundle;

import org.eclipse.core.runtime.Plugin;
import org.osgi.framework.BundleContext;

/**
 * The main plugin class to be used in the desktop.
 */
public class KalypsoServiceCoreClientPlugin extends Plugin
{
  //The shared instance.
  private static KalypsoServiceCoreClientPlugin plugin;
  //Resource bundle.
  private ResourceBundle resourceBundle;
  
  /** factory for webservice proxy for the kalypso client */
  private ProxyFactory m_proxyFactory;

  /**
   * The constructor.
   */
  public KalypsoServiceCoreClientPlugin()
  {
    super();
    plugin = this;
    try
    {
      resourceBundle = ResourceBundle.getBundle( "org.kalypso.core.client.KalypsoServiceClientCorePluginResources" );
    }
    catch( MissingResourceException x )
    {
      resourceBundle = null;
    }
  }

  /**
   * This method is called upon plug-in activation
   */
  public void start( BundleContext context ) throws Exception
  {
    super.start( context );
  }

  /**
   * This method is called when the plug-in is stopped
   */
  public void stop( BundleContext context ) throws Exception
  {
    super.stop( context );
  }

  /**
   * Returns the shared instance.
   */
  public static KalypsoServiceCoreClientPlugin getDefault()
  {
    return plugin;
  }

  /**
   * Returns the string from the plugin's resource bundle, or 'key' if not found.
   */
  public static String getResourceString( String key )
  {
    ResourceBundle bundle = KalypsoServiceCoreClientPlugin.getDefault().getResourceBundle();
    try
    {
      return ( bundle != null ) ? bundle.getString( key ) : key;
    }
    catch( MissingResourceException e )
    {
      return key;
    }
  }

  /**
   * Returns the plugin's resource bundle,
   */
  public ResourceBundle getResourceBundle()
  {
    return resourceBundle;
  }
  
  public void configureProxies( final Properties props )
  {
    // this is the base classname (actually just package name) of all the
    // kalypso service proxies
    props.setProperty( ProxyFactory.KALYPSO_PROXY_BASE, "org.kalypso.services.proxy" );

    m_proxyFactory = new ProxyFactory( props );
  }

  public ProxyFactory getProxyFactory()
  {
    return m_proxyFactory;
  }
}
