package org.kalypso.dss;

import org.eclipse.ui.plugin.*;
import org.osgi.framework.BundleContext;
import java.util.*;

/**
 * The main plugin class to be used in the desktop.
 */
public class KalypsoDSSPlugin extends AbstractUIPlugin
{

  public static final String PLUGIN_ID = "org.kalypso.dss";
  
  public static final String START_URL = "file://d:/temp/perspectives/DssPlanerClient.htm";
  
  //The shared instance.
  private static KalypsoDSSPlugin THE_PLUGIN;

  //Resource bundle.
  private ResourceBundle resourceBundle;

  public final static String MEASUER_MEMBER = "measuresMember";

  /**
   * The constructor.
   */
  public KalypsoDSSPlugin()
  {
    super();
    THE_PLUGIN = this;
    try
    {
      resourceBundle = ResourceBundle.getBundle( "org.kalypso.dss.KalypsoDSSPluginResources" );
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
  public static KalypsoDSSPlugin getDefault()
  {
    return THE_PLUGIN;
  }

  /**
   * Returns the string from the plugin's resource bundle, or 'key' if not found.
   */
  public static String getResourceString( String key )
  {
    ResourceBundle bundle = KalypsoDSSPlugin.getDefault().getResourceBundle();
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
