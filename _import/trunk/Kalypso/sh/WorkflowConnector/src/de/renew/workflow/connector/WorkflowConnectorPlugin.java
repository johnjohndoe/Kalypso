package de.renew.workflow.connector;

import org.eclipse.core.runtime.Plugin;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;
import org.osgi.framework.BundleContext;

import de.renew.workflow.connector.worklist.TaskExecutionListener;

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
   * Returns the shared instance
   * 
   * @return the shared instance
   */
  public static WorkflowConnectorPlugin getDefault( )
  {
    return plugin;
  }
}
