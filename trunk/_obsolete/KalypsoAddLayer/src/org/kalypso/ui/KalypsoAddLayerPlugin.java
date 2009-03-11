package org.kalypso.ui;

import java.util.MissingResourceException;
import java.util.ResourceBundle;

import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

/**
 * The main plugin class to be used in the desktop.
 */
public class KalypsoAddLayerPlugin extends AbstractUIPlugin
{
  // The shared instance.
  private static KalypsoAddLayerPlugin plugin;

  // Resource bundle.
  private ResourceBundle resourceBundle;

  /** Constant for all Kalypso data import wizards (Extenstion point schema org.kalypso.ui.wizard.dataImportWizard.exsd) */
  public static final String PL_IMPORT = "addLayerWizard";

  /**
   * The constructor.
   */
  public KalypsoAddLayerPlugin( )
  {
    super();
    plugin = this;
    try
    {
      resourceBundle = ResourceBundle.getBundle( "org.kalypso.ui.KaylpsoAddLayerPluginPluginResources" );
    }
    catch( MissingResourceException x )
    {
      resourceBundle = null;
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
  public static KalypsoAddLayerPlugin getDefault( )
  {
    return plugin;
  }

  /**
   * Returns the string from the plugin's resource bundle, or 'key' if not found.
   */
  public static String getResourceString( String key )
  {
    ResourceBundle bundle = KalypsoAddLayerPlugin.getDefault().getResourceBundle();
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

  public static String getId( )
  {
    return getDefault().getBundle().getSymbolicName();
  }
}
