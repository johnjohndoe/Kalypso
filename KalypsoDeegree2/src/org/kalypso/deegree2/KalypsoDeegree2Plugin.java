package org.kalypso.deegree2;

import org.eclipse.core.runtime.Plugin;
import org.osgi.framework.BundleContext;

/**
 * The activator class controls the plug-in life cycle.
 */
public class KalypsoDeegree2Plugin extends Plugin
{

  /**
   * The plug-in ID.
   */
  public static final String PLUGIN_ID = "org.kalypso.degree2";

  /**
   * The shared instance.
   */
  private static KalypsoDeegree2Plugin plugin;

  /**
   * The constructor.
   */
  public KalypsoDeegree2Plugin( )
  {
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.eclipse.core.runtime.Plugins#start(org.osgi.framework.BundleContext)
   */
  @Override
  public void start( BundleContext context ) throws Exception
  {
    super.start( context );

    plugin = this;
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.eclipse.core.runtime.Plugin#stop(org.osgi.framework.BundleContext)
   */
  @Override
  public void stop( BundleContext context ) throws Exception
  {
    plugin = null;

    super.stop( context );
  }

  /**
   * Returns the shared instance.
   * 
   * @return The shared instance.
   */
  public static KalypsoDeegree2Plugin getDefault( )
  {
    return plugin;
  }
}