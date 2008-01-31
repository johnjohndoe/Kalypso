package org.kalypso.afgui;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Properties;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchListener;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;
import org.eclipse.ui.handlers.IHandlerService;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.kalypso.afgui.scenarios.PerspectiveWatcher;
import org.kalypso.afgui.scenarios.Scenario;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.afgui.scenarios.TaskExecutionAuthority;
import org.kalypso.afgui.scenarios.TaskExecutor;
import org.kalypso.kalypsosimulationmodel.core.modeling.IModel;
import org.osgi.framework.BundleContext;

import de.renew.workflow.cases.Case;
import de.renew.workflow.connector.cases.CaseHandlingProjectNature;
import de.renew.workflow.connector.cases.CaseHandlingSourceProvider;
import de.renew.workflow.connector.context.ActiveWorkContext;
import de.renew.workflow.connector.worklist.ITaskExecutor;
import de.renew.workflow.connector.worklist.TaskExecutionListener;
import de.renew.workflow.contexts.WorkflowContextHandlerFactory;

/**
 * The activator class controls the plug-in life cycle
 */
public class KalypsoAFGUIFrameworkPlugin extends AbstractUIPlugin
{
  // The plug-in ID
  public static final String PLUGIN_ID = "org.kalypso.afgui"; //$NON-NLS-1$

  // The shared instance
  private static KalypsoAFGUIFrameworkPlugin plugin;

  private static final String ACTIVE_WORKCONTEXT_MEMENTO = "activeWorkContext";

  private PerspectiveWatcher<Scenario> m_perspectiveWatcher;

  private ActiveWorkContext<Scenario> m_activeWorkContext;

  private CaseHandlingSourceProvider<Scenario, IModel> m_szenarioSourceProvider;

  private SzenarioDataProvider m_szenarioDataProvider;

  private TaskExecutionAuthority m_taskExecutionAuthority;

  private ITaskExecutor m_taskExecutor;

  private TaskExecutionListener m_taskExecutionListener;

  public KalypsoAFGUIFrameworkPlugin( )
  {
    plugin = this;
  }

  /**
   * @see org.eclipse.ui.plugin.AbstractUIPlugin#start(org.osgi.framework.BundleContext)
   */
  @Override
  public void start( final BundleContext context ) throws Exception
  {
    super.start( context );

    final WorkflowContextHandlerFactory workflowContextHandlerFactory = new WorkflowContextHandlerFactory();
    final IWorkbench workbench = PlatformUI.getWorkbench();
    final IHandlerService handlerService = (IHandlerService) workbench.getService( IHandlerService.class );
    final ICommandService commandService = (ICommandService) workbench.getService( ICommandService.class );
    m_taskExecutionListener = new TaskExecutionListener();
    commandService.addExecutionListener( m_taskExecutionListener );
    m_taskExecutionAuthority = new TaskExecutionAuthority();
    m_taskExecutor = new TaskExecutor( workflowContextHandlerFactory, m_taskExecutionAuthority, commandService, handlerService );

    PlatformUI.getWorkbench().addWorkbenchListener( new IWorkbenchListener()
    {
      /**
       * @see org.eclipse.ui.IWorkbenchListener#postShutdown(org.eclipse.ui.IWorkbench)
       */
      public void postShutdown( final IWorkbench workbench2 )
      {
      }

      /**
       * @see org.eclipse.ui.IWorkbenchListener#preShutdown(org.eclipse.ui.IWorkbench, boolean)
       */
      @SuppressWarnings("synthetic-access")
      public boolean preShutdown( final IWorkbench workbench2, final boolean forced )
      {
        if( !forced && m_taskExecutionAuthority.canStopTask( m_taskExecutor.getActiveTask() ) )
        {
          m_taskExecutor.stopActiveTask();
          stopSzenarioSourceProvider();
          return true;
        }
        else
          return false;
      }
    } );
  }

  private void startActiveWorkContext( )
  {
    if( m_activeWorkContext == null )
    {
      final Properties properties = new Properties();
      final String fileName = getStateLocation().append( ACTIVE_WORKCONTEXT_MEMENTO ).toOSString();
      final File file = new File( fileName );
      if( file.exists() )
      {
        try
        {
          properties.loadFromXML( new FileInputStream( file ) );
        }
        catch( final Throwable e )
        {
          e.printStackTrace();
        }
      }
      m_activeWorkContext = new ActiveWorkContext<Scenario>( properties, ScenarioHandlingProjectNature.ID );
      m_perspectiveWatcher = new PerspectiveWatcher<Scenario>( m_activeWorkContext.getCurrentCase() );
      m_activeWorkContext.addActiveContextChangeListener( m_perspectiveWatcher );
    }
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
      final IHandlerService handlerService = (IHandlerService) workbench.getService( IHandlerService.class );
      if( handlerService != null )
      {
        handlerService.removeSourceProvider( m_szenarioSourceProvider );
      }
      final ICommandService commandService = (ICommandService) workbench.getService( ICommandService.class );
      if( commandService != null )
      {
        commandService.removeExecutionListener( m_taskExecutionListener );
      }
    }

    plugin = null;
    super.stop( context );
  }

  public ActiveWorkContext<Scenario> getActiveWorkContext( )
  {
    startActiveWorkContext();
    startSzenarioSourceProvider();
    return m_activeWorkContext;
  }

  public ITaskExecutor getTaskExecutor( )
  {
    return m_taskExecutor;
  }

  public TaskExecutionAuthority getTaskExecutionAuthority( )
  {
    return m_taskExecutionAuthority;
  }

  public SzenarioDataProvider getDataProvider( )
  {
    return m_szenarioDataProvider;
  }

  /**
   * Returns the shared instance
   * 
   * @return the shared instance
   */
  public static KalypsoAFGUIFrameworkPlugin getDefault( )
  {
    return plugin;
  }

  /**
   * Returns an image descriptor for the image file at the given plug-in relative path
   * 
   * @param path
   *            the path
   * @return the image descriptor
   */
  public static ImageDescriptor getImageDescriptor( final String path )
  {
    return imageDescriptorFromPlugin( PLUGIN_ID, path );
  }

  private void startSzenarioSourceProvider( )
  {
    if( m_szenarioSourceProvider == null )
    {
      // This can only be called if the platform has already been started
      final IWorkbench workbench = PlatformUI.getWorkbench();
      final IHandlerService handlerService = (IHandlerService) workbench.getService( IHandlerService.class );
      m_szenarioDataProvider = new SzenarioDataProvider();
      m_szenarioSourceProvider = new CaseHandlingSourceProvider<Scenario, IModel>( m_activeWorkContext, m_szenarioDataProvider );
      handlerService.addSourceProvider( m_szenarioSourceProvider );
    }
  }

  private void stopSzenarioSourceProvider( )
  {
    final Properties properties = createProperties();
    final String fileName = getStateLocation().append( ACTIVE_WORKCONTEXT_MEMENTO ).toOSString();
    final File file = new File( fileName );
    if( file.exists() )
    {
      file.delete();
    }
    try
    {
      properties.storeToXML( new FileOutputStream( file ), "" );
    }
    catch( final FileNotFoundException e1 )
    {
      e1.printStackTrace();
    }
    catch( final IOException e1 )
    {
      e1.printStackTrace();
    }

    if( PlatformUI.isWorkbenchRunning() )
    {
      try
      {
        m_activeWorkContext.setCurrentCase( null );
      }
      catch( final CoreException e )
      {
        e.printStackTrace();
      }
      m_activeWorkContext.removeActiveContextChangeListener( m_perspectiveWatcher );
    }
  }

  /**
   * Creates properties that contain current project and case information for restoring state later
   */
  private Properties createProperties( )
  {
    final Properties properties = new Properties();
    final CaseHandlingProjectNature currentProject = m_activeWorkContext.getCurrentProject();
    if( currentProject != null )
    {
      final IProject project = currentProject.getProject();
      if( project != null )
      {
        final String projectPath = project.getName();
        properties.put( ActiveWorkContext.MEMENTO_PROJECT, projectPath );
      }
      final Case currentCase = m_activeWorkContext.getCurrentCase();
      if( currentCase != null )
      {
        final String caseString = currentCase.getURI();
        properties.put( ActiveWorkContext.MEMENTO_CASE, caseString );
      }
    }
    return properties;
  }
}
