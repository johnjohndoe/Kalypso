package de.renew.workflow.connector;

import org.eclipse.core.runtime.Plugin;
import org.osgi.framework.BundleContext;

/**
 * The activator class controls the plug-in life cycle
 */
public class WorkflowConnectorPlugin extends Plugin
{

  // The plug-in ID
  public static final String PLUGIN_ID = "de.renew.workflow.connector";

  private static WorkflowConnectorPlugin plugin;

  /**
   * The constructor
   */
  public WorkflowConnectorPlugin( )
  {
    plugin = this;
  }

  /**
   * @see org.eclipse.ui.plugin.AbstractUIPlugin#start(org.osgi.framework.BundleContext)
   */
  @Override
  public void start( BundleContext context ) throws Exception
  {
    super.start( context );
  }

  /**
   * @see org.eclipse.ui.plugin.AbstractUIPlugin#stop(org.osgi.framework.BundleContext)
   */
  @Override
  public void stop( BundleContext context ) throws Exception
  {
    plugin = null;
    super.stop( context );
  }

  /**
   * Returns the shared instance
   * 
   * @return the shared instance
   */
  public static WorkflowConnectorPlugin getDefault( )
  {
    return plugin;
  }

  // private static boolean isRenewRunning() {
  // return PluginManager.getLoaderLocation() != null;
  // }

  // public void checkRunning() {
  // if (!isRenewRunning()) {
  // WorkflowServer.start(null);
  // }
  // }
}
