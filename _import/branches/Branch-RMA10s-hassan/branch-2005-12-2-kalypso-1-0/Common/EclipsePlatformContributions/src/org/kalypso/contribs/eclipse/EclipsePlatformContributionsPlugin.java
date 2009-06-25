package org.kalypso.contribs.eclipse;

import java.util.MissingResourceException;
import java.util.ResourceBundle;

import org.eclipse.core.runtime.Plugin;
import org.osgi.framework.BundleContext;

/**
 * The main plugin class to be used in the desktop.
 */
public class EclipsePlatformContributionsPlugin extends Plugin
{
  // The shared instance.
  private static EclipsePlatformContributionsPlugin plugin;

  // Resource bundle.
  private ResourceBundle resourceBundle;

  public static String getID()
  {
    return getDefault().getBundle().getSymbolicName();
  }
  
  public EclipsePlatformContributionsPlugin()
  {
    super();
    plugin = this;
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
    plugin = null;
    resourceBundle = null;
  }

  /**
   * Returns the shared instance.
   */
  public static EclipsePlatformContributionsPlugin getDefault()
  {
    return plugin;
  }

  /**
   * Returns the string from the plugin's resource bundle, or 'key' if not found.
   */
  public static String getResourceString( String key )
  {
    ResourceBundle bundle = EclipsePlatformContributionsPlugin.getDefault().getResourceBundle();
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
    try
    {
      if( resourceBundle == null )
        resourceBundle = ResourceBundle
            .getBundle( "org.kalypso.contribs.eclipse.platform.EclipsePlatformContributionsPluginResources" );
    }
    catch( MissingResourceException x )
    {
      resourceBundle = null;
    }
    return resourceBundle;
  }
}
