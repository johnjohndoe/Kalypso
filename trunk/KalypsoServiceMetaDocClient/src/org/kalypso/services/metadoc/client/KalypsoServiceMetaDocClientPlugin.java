package org.kalypso.services.metadoc.client;

import java.util.MissingResourceException;
import java.util.ResourceBundle;

import org.eclipse.core.runtime.Plugin;

/**
 * The main plugin class to be used in the desktop.
 */
public class KalypsoServiceMetaDocClientPlugin extends Plugin
{
  //The shared instance.
  private static KalypsoServiceMetaDocClientPlugin plugin;
  //Resource bundle.
  private ResourceBundle resourceBundle;

  /**
   * The constructor.
   */
  public KalypsoServiceMetaDocClientPlugin()
  {
    super();
    plugin = this;
    try
    {
      resourceBundle = ResourceBundle
          .getBundle( "org.kalypso.metadoc.client.KalypsoServiceMetaDocClientPluginResources" );
    }
    catch( MissingResourceException x )
    {
      resourceBundle = null;
    }
  }

  /**
   * Returns the shared instance.
   */
  public static KalypsoServiceMetaDocClientPlugin getDefault()
  {
    return plugin;
  }

  /**
   * Returns the string from the plugin's resource bundle, or 'key' if not found.
   */
  public static String getResourceString( String key )
  {
    ResourceBundle bundle = KalypsoServiceMetaDocClientPlugin.getDefault().getResourceBundle();
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

  public static String getID()
  {
    return getDefault().getBundle().getSymbolicName();
  }
}
