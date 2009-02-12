package org.kalypso.services.user.client;

import java.util.MissingResourceException;
import java.util.ResourceBundle;

import javax.xml.rpc.ServiceException;

import org.eclipse.core.runtime.Plugin;
import org.kalypso.core.client.KalypsoServiceCoreClientPlugin;
import org.kalypso.services.user.impl.KalypsoUserService;

/**
 * The main plugin class to be used in the desktop.
 */
public class KalypsoServiceUserClientPlugin extends Plugin
{
  // The shared instance.
  private static KalypsoServiceUserClientPlugin plugin;

  // Resource bundle.
  private ResourceBundle resourceBundle;

  /**
   * The constructor.
   */
  public KalypsoServiceUserClientPlugin( )
  {
    super();
    plugin = this;
    try
    {
      resourceBundle = ResourceBundle.getBundle( "org.kalypso.services.user.client.KalypsoClientUserPluginResources" );
    }
    catch( MissingResourceException x )
    {
      resourceBundle = null;
    }
  }

  /**
   * Returns the shared instance.
   */
  public static KalypsoServiceUserClientPlugin getDefault( )
  {
    return plugin;
  }

  /**
   * Returns the string from the plugin's resource bundle, or 'key' if not found.
   */
  public static String getResourceString( String key )
  {
    ResourceBundle bundle = KalypsoServiceUserClientPlugin.getDefault().getResourceBundle();
    try
    {
      return (bundle != null) ? bundle.getString( key ) : key;
    }
    catch( MissingResourceException e )
    {
      return key;
    }
  }

  /**
   * Returns the plugin's resource bundle,
   */
  public ResourceBundle getResourceBundle( )
  {
    return resourceBundle;
  }

  /**
   * Convenience method that returns the user service proxy
   * 
   * @return WebService proxy for the IUserService
   */
  public KalypsoUserService getUserServiceProxy( ) throws ServiceException
  {
    return (KalypsoUserService) KalypsoServiceCoreClientPlugin.getDefault().getProxyFactory().getAnyProxy( "Kalypso_UserService", "IUserService" );
  }
}
