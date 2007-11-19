package de.renew.workflow.connector;

import org.eclipse.core.runtime.Plugin;

/**
 * The activator class controls the plug-in life cycle
 */
public class WorkflowConnectorPlugin extends Plugin
{
  // The plug-in ID
  public static final String PLUGIN_ID = "de.renew.workflow.connector"; //$NON-NLS-1$

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
