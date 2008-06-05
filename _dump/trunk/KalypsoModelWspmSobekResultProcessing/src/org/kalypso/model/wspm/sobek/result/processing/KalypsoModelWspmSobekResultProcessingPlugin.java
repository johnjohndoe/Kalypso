package org.kalypso.model.wspm.sobek.result.processing;

import org.eclipse.core.runtime.Plugin;
import org.osgi.framework.BundleContext;

/**
 * The activator class controls the plug-in life cycle
 */
public class KalypsoModelWspmSobekResultProcessingPlugin extends Plugin
{

  // The plug-in ID
  public static final String PLUGIN_ID = "org.kalypso.model.wspm.sobek.result.processing";

  // The shared instance
  private static KalypsoModelWspmSobekResultProcessingPlugin plugin;

  /**
   * The constructor
   */
  public KalypsoModelWspmSobekResultProcessingPlugin( )
  {
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.eclipse.core.runtime.Plugins#start(org.osgi.framework.BundleContext)
   */
  @Override
  public void start( final BundleContext context ) throws Exception
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
  public void stop( final BundleContext context ) throws Exception
  {
    plugin = null;
    super.stop( context );
  }

  /**
   * Returns the shared instance
   * 
   * @return the shared instance
   */
  public static KalypsoModelWspmSobekResultProcessingPlugin getDefault( )
  {
    return plugin;
  }

}
