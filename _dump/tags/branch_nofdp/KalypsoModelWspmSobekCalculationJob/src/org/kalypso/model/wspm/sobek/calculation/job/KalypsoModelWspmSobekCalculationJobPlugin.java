package org.kalypso.model.wspm.sobek.calculation.job;

import org.eclipse.core.runtime.Plugin;
import org.osgi.framework.BundleContext;

/**
 * The activator class controls the plug-in life cycle
 */
public class KalypsoModelWspmSobekCalculationJobPlugin extends Plugin
{

  // The plug-in ID
  public static final String PLUGIN_ID = "org.kalypso.model.wspm.sobek.calculation.job"; //$NON-NLS-1$

  // The shared instance
  private static KalypsoModelWspmSobekCalculationJobPlugin plugin;

  /**
   * The constructor
   */
  public KalypsoModelWspmSobekCalculationJobPlugin( )
  {
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.eclipse.ui.plugin.AbstractUIPlugin#start(org.osgi.framework.BundleContext)
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
   * @see org.eclipse.ui.plugin.AbstractUIPlugin#stop(org.osgi.framework.BundleContext)
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
  public static KalypsoModelWspmSobekCalculationJobPlugin getDefault( )
  {
    return plugin;
  }

}
