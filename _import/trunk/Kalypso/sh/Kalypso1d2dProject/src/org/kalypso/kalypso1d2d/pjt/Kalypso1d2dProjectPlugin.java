package org.kalypso.kalypso1d2d.pjt;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.URL;
import java.util.Properties;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.ImageRegistry;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchListener;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;
import org.eclipse.ui.handlers.IHandlerService;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.kalypso.afgui.ScenarioHandlingProjectNature;
import org.kalypso.afgui.scenarios.Scenario;
import org.kalypso.commons.eclipse.core.runtime.PluginImageProvider;
import org.kalypso.kalypso1d2d.pjt.perspective.PerspectiveWatcher;
import org.kalypso.kalypso1d2d.pjt.views.SzenarioDataProvider;
import org.kalypso.kalypso1d2d.pjt.views.TaskExecutionAuthority;
import org.kalypso.kalypso1d2d.pjt.views.TaskExecutor;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
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
public class Kalypso1d2dProjectPlugin extends AbstractUIPlugin
{

  // The plug-in ID
  public static final String PLUGIN_ID = "org.eclipse.kalypso1d2d.pjt.Kalypso1d2dProject";

  private static final String ACTIVE_WORKCONTEXT_MEMENTO = "activeWorkContext";

  // The shared instance
  private static Kalypso1d2dProjectPlugin plugin;

  private static final String ICON_SIM_MODEL_PATH = "/icons/nuvola_select/ledblue.png";

  public static final String KEY_ICON_SIM_MODEL = "_ICON_SIM_MODEL_";

  private PerspectiveWatcher<Scenario> m_perspectiveWatcher;

  private ActiveWorkContext<Scenario> m_activeWorkContext;

  private CaseHandlingSourceProvider<Scenario, IFeatureWrapper2> m_szenarioSourceProvider;

  private SzenarioDataProvider m_szenarioDataProvider;

  private TaskExecutionAuthority m_taskExecutionAuthority;

  private ITaskExecutor m_taskExecutor;

  private TaskExecutionListener m_taskExecutionListener;

  private PluginImageProvider m_imageProvider;

  /**
   * The constructor
   */
  public Kalypso1d2dProjectPlugin( )
  {
    plugin = this;
  }

  public ActiveWorkContext<Scenario> getActiveWorkContext( )
  {
    return m_activeWorkContext;
  }

  /**
   * @see org.eclipse.ui.plugin.AbstractUIPlugin#start(org.osgi.framework.BundleContext)
   */
  @Override
  public void start( BundleContext context ) throws Exception
  {
    super.start( context );

    startSzenarioSourceProvider();

    final WorkflowContextHandlerFactory workflowContextHandlerFactory = new WorkflowContextHandlerFactory();
    final IWorkbench workbench = PlatformUI.getWorkbench();
    final IHandlerService handlerService = (IHandlerService) workbench.getService( IHandlerService.class );
    final ICommandService commandService = (ICommandService) workbench.getService( ICommandService.class );
    m_taskExecutionListener = new TaskExecutionListener();
    commandService.addExecutionListener( m_taskExecutionListener );
    m_taskExecutionAuthority = new TaskExecutionAuthority();
    m_taskExecutor = new TaskExecutor( workflowContextHandlerFactory, m_taskExecutionAuthority, commandService, handlerService );

    // delete tmp images both on startup and shutdown
    m_imageProvider = new PluginImageProvider( this );
    m_imageProvider.resetTmpFiles();

    PlatformUI.getWorkbench().addWorkbenchListener( new IWorkbenchListener()
    {
      /**
       * @see org.eclipse.ui.IWorkbenchListener#postShutdown(org.eclipse.ui.IWorkbench)
       */
      public void postShutdown( final IWorkbench workbench )
      {
      }

      /**
       * @see org.eclipse.ui.IWorkbenchListener#preShutdown(org.eclipse.ui.IWorkbench, boolean)
       */
      @SuppressWarnings("synthetic-access")
      public boolean preShutdown( final IWorkbench workbench, final boolean forced )
      {
        if( !forced && m_taskExecutionAuthority.canStopTask( m_taskExecutor.getActiveTask() ) )
        {
          m_taskExecutor.stopActiveTask();
          stopSzenarioSourceProvider();
          return true;
        }
        else
        {
          return false;
        }
      }
    } );
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

    // delete tmp images both on startup and shutdown
    m_imageProvider.resetTmpFiles();
    m_imageProvider = null;

    plugin = null;
    super.stop( context );
  }

  public ITaskExecutor getTaskExecutor( )
  {
    return m_taskExecutor;
  }

  public TaskExecutionAuthority getTaskExecutionAuthority( )
  {
    return m_taskExecutionAuthority;
  }

  /**
   * Returns the shared instance
   * 
   * @return the shared instance
   */
  public static Kalypso1d2dProjectPlugin getDefault( )
  {
    return plugin;
  }

  @Override
  protected void initializeImageRegistry( ImageRegistry reg )
  {
    String couples[][] = { { KEY_ICON_SIM_MODEL, ICON_SIM_MODEL_PATH } };

    // TODO dipose images on stop
    for( String[] curCouple : couples )
    {
      URL url = getBundle().getEntry( curCouple[1] );
      ImageDescriptor desc = ImageDescriptor.createFromURL( url );
      // reg.put(curCouple[0], desc);
      reg.put( KEY_ICON_SIM_MODEL, desc.createImage() );
    }
    return;
  }

  public static Image getImageDescriptor( String key )
  {
    return getDefault().getImageRegistry().get( key );
  }

  public SzenarioDataProvider getDataProvider( )
  {
    return m_szenarioDataProvider;
  }

  private void startSzenarioSourceProvider( )
  {
    if( m_szenarioSourceProvider == null )
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
      // This can only be called if the platform has already been started
      m_activeWorkContext = new ActiveWorkContext<Scenario>( properties, ScenarioHandlingProjectNature.ID );
      final IWorkbench workbench = PlatformUI.getWorkbench();
      final IHandlerService handlerService = (IHandlerService) workbench.getService( IHandlerService.class );
      m_szenarioDataProvider = new SzenarioDataProvider();
      m_szenarioSourceProvider = new CaseHandlingSourceProvider<Scenario, IFeatureWrapper2>( m_activeWorkContext, m_szenarioDataProvider );
      handlerService.addSourceProvider( m_szenarioSourceProvider );
      m_perspectiveWatcher = new PerspectiveWatcher<Scenario>( m_activeWorkContext.getCurrentCase() );
      m_activeWorkContext.addActiveContextChangeListener( m_perspectiveWatcher );
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
    catch( FileNotFoundException e1 )
    {
      e1.printStackTrace();
    }
    catch( IOException e1 )
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

  public static PluginImageProvider getImageProvider( )
  {
    return getDefault().m_imageProvider;
  }

}
