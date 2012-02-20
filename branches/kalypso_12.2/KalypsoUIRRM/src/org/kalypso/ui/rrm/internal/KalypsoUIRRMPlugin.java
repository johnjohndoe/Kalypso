package org.kalypso.ui.rrm.internal;

import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

/**
 * The main plugin class to be used in the desktop.
 */
public class KalypsoUIRRMPlugin extends AbstractUIPlugin
{
  // The shared instance.
  private static KalypsoUIRRMPlugin plugin;

  /**
   * The constructor.
   */
  public KalypsoUIRRMPlugin( )
  {
    plugin = this;
  }

  /**
   * This method is called upon plug-in activation
   */
  @Override
  public void start( final BundleContext context ) throws Exception
  {
    super.start( context );
  }

  /**
   * This method is called when the plug-in is stopped
   */
  @Override
  public void stop( final BundleContext context ) throws Exception
  {
    super.stop( context );
    plugin = null;
  }

  /**
   * Returns the shared instance.
   */
  public static KalypsoUIRRMPlugin getDefault( )
  {
    return plugin;
  }

  public static String getID( )
  {
    return getDefault().getBundle().getSymbolicName();
  }
}
