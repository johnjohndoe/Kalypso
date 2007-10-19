package org.kalypso.wss;

import org.eclipse.core.runtime.Plugin;
import org.osgi.framework.BundleContext;

/**
 * The activator class controls the plug-in life cycle.
 */
public class KalypsoWSSPlugin extends Plugin
{

  /**
   * The plug-in ID.
   */
  private static final String PLUGIN_ID = "org.kalypso.wss";

  /**
   * The shared instance.
   */
  private static KalypsoWSSPlugin plugin;

  /**
   * The constructor.
   */
  public KalypsoWSSPlugin( )
  {
  }

  /**
   * @see org.eclipse.core.runtime.Plugin#start(org.osgi.framework.BundleContext)
   */
  @Override
  public void start( BundleContext context ) throws Exception
  {
    super.start( context );

    plugin = this;
  }

  /**
   * @see org.eclipse.core.runtime.Plugin#stop(org.osgi.framework.BundleContext)
   */
  @Override
  public void stop( BundleContext context ) throws Exception
  {
    plugin = null;

    super.stop( context );
  }

  /**
   * This function returns the plug-in ID.
   * 
   * @return The plug-in ID.
   */
  public String getID( )
  {
    return PLUGIN_ID;
  }

  /**
   * Returns the shared instance.
   * 
   * @return The shared instance.
   */
  public static KalypsoWSSPlugin getDefault( )
  {
    return plugin;
  }
}