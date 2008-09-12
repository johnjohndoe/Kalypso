package org.kalypso.dss;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

/**
 * The main plugin class to be used in the desktop.
 */
public class KalypsoDSSPlugin extends AbstractUIPlugin
{

  public static final String PLUGIN_ID = "org.kalypso.dss";
  
  public static URL START_URL;// = "file://d:/eclipse3.1/runtime-workspace/";
  
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
    IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
    try
    {
      START_URL = root.getLocation().toFile().toURL();
      resourceBundle = ResourceBundle.getBundle( "org.kalypso.dss.KalypsoDSSPluginResources" );
    }
    catch( MissingResourceException x )
    {
      resourceBundle = null;
    }
    catch( MalformedURLException e )
    {
      e.printStackTrace();
      START_URL = null;
    }
  }

  /**
   * This method is called upon plug-in activation
   */
  @Override
  public void start( BundleContext context ) throws Exception
  {
    super.start( context );
  }

  /**
   * This method is called when the plug-in is stopped
   */
  @Override
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
