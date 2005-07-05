package org.kalypso.contribs.eclipse;

import org.eclipse.core.runtime.Plugin;
import org.osgi.framework.BundleContext;
import java.util.*;

/**
 * The main plugin class to be used in the desktop.
 */
public class EclipseRCPContributionsPlugin extends Plugin
{
  public static final String ID = "org.kalypso.contribs.eclipsercp";
  
  //The shared instance.
  private static EclipseRCPContributionsPlugin plugin;

  //Resource bundle.
  private ResourceBundle resourceBundle;

  public static String getID()
  {
    return getDefault().getBundle().getSymbolicName();
  }
  
  /**
   * The constructor.
   */
  public EclipseRCPContributionsPlugin()
  {
    super();
    plugin = this;
    try
    {
      resourceBundle = ResourceBundle.getBundle( "org.kalypso.contribs.eclipse.EclipseRCPContributionsPluginResources" );
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
  public static EclipseRCPContributionsPlugin getDefault()
  {
    return plugin;
  }

  /**
   * Returns the string from the plugin's resource bundle, or 'key' if not found.
   */
  public static String getResourceString( String key )
  {
    ResourceBundle bundle = EclipseRCPContributionsPlugin.getDefault().getResourceBundle();
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
}
