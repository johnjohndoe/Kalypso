package org.kalypso.kml.export;

import org.eclipse.core.runtime.Plugin;
import org.osgi.framework.BundleContext;

public class KalypsoKMLPlugin extends Plugin
{
// The shared instance.
  private static KalypsoKMLPlugin plugin = null;

  /**
   * Returns the shared instance.
   */
  public static KalypsoKMLPlugin getDefault( )
  {
    return KalypsoKMLPlugin.plugin;
  }

  /**
   *
   */
  public KalypsoKMLPlugin( )
  {
    KalypsoKMLPlugin.plugin = this;
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
    KalypsoKMLPlugin.plugin = null;
  }

}
