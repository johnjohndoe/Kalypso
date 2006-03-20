package org.kalypso.metadoc;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.ui.plugin.*;
import org.kalypso.metadoc.impl.MetadocExtensions;
import org.osgi.framework.BundleContext;
import java.util.*;

/**
 * The main plugin class to be used in the desktop.
 */
public class KalypsoMetaDocPlugin extends AbstractUIPlugin
{
  //The shared instance.
  private static KalypsoMetaDocPlugin plugin;
  //Resource bundle.
  private ResourceBundle resourceBundle;
  private IExportTarget[] m_targets;

  /**
   * The constructor.
   */
  public KalypsoMetaDocPlugin()
  {
    super();
    plugin = this;
    try
    {
      resourceBundle = ResourceBundle.getBundle( "org.kalypso.metadoc.KalypsoMetaDocPluginResources" );
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
  public static KalypsoMetaDocPlugin getDefault()
  {
    return plugin;
  }

  /**
   * Returns the string from the plugin's resource bundle, or 'key' if not found.
   */
  public static String getResourceString( String key )
  {
    ResourceBundle bundle = KalypsoMetaDocPlugin.getDefault().getResourceBundle();
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

  public static String getId()
  {
    return getDefault().getBundle().getSymbolicName();
  }

  public IExportTarget[] getTargets() throws CoreException
  {
    if( m_targets == null )
      m_targets = MetadocExtensions.retrieveTargets();

    return m_targets;
  }
}
