package org.kalypso.services.calculation.local;

import java.util.MissingResourceException;
import java.util.ResourceBundle;

import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

/**
 * The main plugin class to be used in the desktop.
 */
public class KalypsoLocalCalulationPlugin extends AbstractUIPlugin
{
  //The shared instance.
  private static KalypsoLocalCalulationPlugin plugin;

  //Resource bundle.
  private ResourceBundle resourceBundle;

  /**
   * The constructor.
   */
  public KalypsoLocalCalulationPlugin()
  {
    super();
    plugin = this;
    try
    {
      resourceBundle = ResourceBundle
          .getBundle( "org.kalypso.services.calculation.local.KalypsoLocalCalulationPluginResources" );
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
  public static KalypsoLocalCalulationPlugin getDefault()
  {
    return plugin;
  }

  /**
   * Returns the string from the plugin's resource bundle, or 'key' if not found.
   */
  public static String getResourceString( String key )
  {
    ResourceBundle bundle = KalypsoLocalCalulationPlugin.getDefault().getResourceBundle();
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

  public String getId()
  {
    return getBundle().getSymbolicName();
  }
}
