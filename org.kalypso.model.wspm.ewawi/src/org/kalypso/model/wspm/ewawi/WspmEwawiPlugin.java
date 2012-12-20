package org.kalypso.model.wspm.ewawi;

import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

/**
 * The activator class controls the plug-in life cycle
 */
public class WspmEwawiPlugin extends AbstractUIPlugin
{
  // The plug-in ID
  public static final String PLUGIN_ID = "org.kalypso.model.wspm.ewawi"; //$NON-NLS-1$

  // The shared instance
  private static WspmEwawiPlugin plugin;

  @Override
  public void start( final BundleContext context ) throws Exception
  {
    super.start( context );
    plugin = this;
  }

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
  public static WspmEwawiPlugin getDefault( )
  {
    return plugin;
  }
}