package org.kalypso.risk.plugin;

import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.kalypso.commons.eclipse.core.runtime.PluginImageProvider;
import org.osgi.framework.BundleContext;

import de.renew.workflow.connector.worklist.TaskExecutionAuthority;
import de.renew.workflow.connector.worklist.TaskExecutionListener;

/**
 * The activator class controls the plug-in life cycle
 */
public class KalypsoRiskPlugin extends AbstractUIPlugin
{
  private static KalypsoRiskPlugin m_plugin;

  private TaskExecutionAuthority m_taskExecutionAuthority;

  private TaskExecutionListener m_taskExecutionListener;

  private PluginImageProvider m_imageProvider;

  public KalypsoRiskPlugin( )
  {
    m_plugin = this;
  }

  public static KalypsoRiskPlugin getDefault( )
  {
    return m_plugin;
  }

  @Override
  public void start( BundleContext context ) throws Exception
  {
    super.start( context );

    final IWorkbench workbench = PlatformUI.getWorkbench();
    final ICommandService commandService = (ICommandService) workbench.getService( ICommandService.class );
    m_taskExecutionListener = new TaskExecutionListener();
    commandService.addExecutionListener( m_taskExecutionListener );
    m_taskExecutionAuthority = new TaskExecutionAuthority();

    // delete tmp images both on startup and shutdown
    m_imageProvider = new PluginImageProvider( this );
    m_imageProvider.resetTmpFiles();
  }

  /**
   * @see org.eclipse.ui.plugin.AbstractUIPlugin#stop(org.osgi.framework.BundleContext)
   */
  @Override
  public void stop( final BundleContext context ) throws Exception
  {
    final IWorkbench workbench = PlatformUI.getWorkbench();
    if( !workbench.isClosing() )
    {
      final ICommandService commandService = (ICommandService) workbench.getService( ICommandService.class );
      if( commandService != null )
      {
        commandService.removeExecutionListener( m_taskExecutionListener );
      }
    }

    // delete tmp images both on startup and shutdown
    m_imageProvider.resetTmpFiles();
    m_imageProvider = null;

    m_plugin = null;
    super.stop( context );
  }

  public TaskExecutionAuthority getTaskExecutionAuthority( )
  {
    return m_taskExecutionAuthority;
  }
}
