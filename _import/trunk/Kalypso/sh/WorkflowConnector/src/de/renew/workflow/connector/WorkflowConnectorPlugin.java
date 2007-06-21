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

  private TaskExecutionListener m_taskExecutionListener;

  /**
   * The constructor
   */
  public WorkflowConnectorPlugin( )
  {
    plugin = this;
  }

  /**
   * @see org.eclipse.core.runtime.Plugin#start(org.osgi.framework.BundleContext)
   */
  @Override
  public void start( BundleContext context ) throws Exception
  {
    super.start( context );
    final IWorkbench workbench = PlatformUI.getWorkbench();
    final ICommandService commandService = (ICommandService) workbench.getService( ICommandService.class );
    m_taskExecutionListener = new TaskExecutionListener();
    commandService.addExecutionListener( m_taskExecutionListener );
  }

  /**
   * @see org.eclipse.core.runtime.Plugin#stop(org.osgi.framework.BundleContext)
   */
  @Override
  public void stop( BundleContext context ) throws Exception
  {
    final IWorkbench workbench = PlatformUI.getWorkbench();
    if( !workbench.isClosing() )
    {
      final ICommandService commandService = (ICommandService) workbench.getService( ICommandService.class );
      commandService.removeExecutionListener( m_taskExecutionListener );
    }
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
