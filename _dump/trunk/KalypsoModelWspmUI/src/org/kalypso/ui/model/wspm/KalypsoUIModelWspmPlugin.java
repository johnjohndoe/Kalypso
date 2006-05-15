package org.kalypso.ui.model.wspm;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

/**
 * The main plugin class to be used in the desktop.
 */
public class KalypsoUIModelWspmPlugin extends AbstractUIPlugin
{

  // The shared instance.
  private static KalypsoUIModelWspmPlugin plugin;

  /**
   * The constructor.
   */
  public KalypsoUIModelWspmPlugin( )
  {
    plugin = this;
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
    plugin = null;
  }

  /**
   * Returns the shared instance.
   */
  public static KalypsoUIModelWspmPlugin getDefault( )
  {
    return plugin;
  }

  /**
   * Returns an image descriptor for the image file at the given plug-in relative path.
   * 
   * @param path
   *          the path
   * @return the image descriptor
   */
  public static ImageDescriptor getImageDescriptor( String path )
  {
    return AbstractUIPlugin.imageDescriptorFromPlugin( "org.kalypso.ui.model.wspm", path );
  }
}
