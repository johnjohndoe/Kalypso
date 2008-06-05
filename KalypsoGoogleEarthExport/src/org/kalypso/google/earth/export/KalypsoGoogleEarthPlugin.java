package org.kalypso.google.earth.export;

import org.eclipse.core.runtime.Plugin;
import org.osgi.framework.BundleContext;

public class KalypsoGoogleEarthPlugin extends Plugin
{
// The shared instance.
  private static KalypsoGoogleEarthPlugin plugin = null;

  /**
   * Returns the shared instance.
   */
  public static KalypsoGoogleEarthPlugin getDefault( )
  {
    return KalypsoGoogleEarthPlugin.plugin;
  }

  /**
   *
   */
  public KalypsoGoogleEarthPlugin( )
  {
    KalypsoGoogleEarthPlugin.plugin = this;
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
    KalypsoGoogleEarthPlugin.plugin = null;
  }

}
