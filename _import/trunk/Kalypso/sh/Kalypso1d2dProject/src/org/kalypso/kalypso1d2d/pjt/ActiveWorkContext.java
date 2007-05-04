package org.kalypso.kalypso1d2d.pjt;

import java.lang.reflect.InvocationTargetException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.dialogs.ProgressMonitorDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.afgui.scenarios.IScenarioManager;
import org.kalypso.afgui.scenarios.Scenario;
import org.kalypso.afgui.scenarios.ScenarioManager;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.kalypso1d2d.pjt.actions.PerspectiveWatcher;
import org.kalypso.kalypso1d2d.pjt.views.SzenarioDataProvider;
import org.kalypso.ui.wizards.imports.ISzenarioSourceProvider;

import de.renew.workflow.base.Task;
import de.renew.workflow.connector.ITaskExecutionAuthority;

/**
 * Represents the work context for a user. A work context is made of:
 * <ul>
 * <li/>The current project the user is working on <li/>The workflow system <li/>A scenario manager instance
 * </ul>
 * 
 * @author Patrice Congo, Stefan Kurzbach
 */
public class ActiveWorkContext implements ITaskExecutionAuthority
{
  private final static Logger logger = Logger.getLogger( ActiveWorkContext.class.getName() );

  private static final boolean log = Boolean.parseBoolean( Platform.getDebugOption( "org.kalypso.kalypso1d2d.pjt/debug" ) );

  static
  {
    if( !log )
      logger.setUseParentHandlers( false );
  }

  private static final String MEMENTO_PROJECT = "project";

  private static final String MEMENTO_SCENARIO = "scenario";

  private ScenarioManager m_scenarioManager;

  private IProject m_activeProject;

  private final List<IActiveContextChangeListener> activeProjectChangeListener = new ArrayList<IActiveContextChangeListener>();

  /**
   * list of registries where we are registered as listeners <br>
   * used for clean dispose
   */
  private final List<Object> m_registries = new ArrayList<Object>();

  private final SzenarioSourceProvider m_simModelProvider;

  private final PerspectiveWatcher m_perspectiveWatcher = new PerspectiveWatcher();

  public ActiveWorkContext( final Properties properties )
  {
    addActiveContextChangeListener( m_perspectiveWatcher );
    final IWorkbench workbench = PlatformUI.getWorkbench();
    final IWorkbenchWindow window = workbench.getActiveWorkbenchWindow();
    if( window != null )
    {
      window.addPerspectiveListener( m_perspectiveWatcher );
      m_registries.add( window );
    }

    final IHandlerService handlerService = (IHandlerService) workbench.getService( IHandlerService.class );
    m_simModelProvider = new SzenarioSourceProvider( this );
    handlerService.addSourceProvider( m_simModelProvider );
    m_registries.add( handlerService );

    restoreState( properties );
  }

  private void restoreState( final Properties properties )
  {
    final String projectString = properties.getProperty( MEMENTO_PROJECT );
    if( projectString != null )
    {
      final IPath projectPath = Path.fromPortableString( projectString );
      final IResource resource = ResourcesPlugin.getWorkspace().getRoot().findMember( projectPath );
      if( resource != null && resource.getType() == IResource.PROJECT )
      {
        final IProject project = (IProject) resource;
        setActiveProject( project );

        final String scenarioString = properties.getProperty( MEMENTO_SCENARIO );
        if( scenarioString != null )
        {
          final IScenarioManager scenarioManager = getScenarioManager();
          final Scenario scenario = scenarioManager.getScenario( scenarioString );
          setCurrentSzenario( scenario );
        }
      }
    }
  }

  public SzenarioDataProvider getDataProvider( )
  {
    return (SzenarioDataProvider) m_simModelProvider.getCurrentState().get( SzenarioSourceProvider.ACTIVE_SZENARIO_DATA_PROVIDER_NAME );
  }

  public void dispose( )
  {
    // remove all listeners
    for( Iterator iter = m_registries.iterator(); iter.hasNext(); )
    {
      final Object registry = iter.next();
      if( registry instanceof IWorkbenchWindow )
        ((IWorkbenchWindow) registry).removePerspectiveListener( m_perspectiveWatcher );
      if( registry instanceof IHandlerService )
        ((IHandlerService) registry).removeSourceProvider( m_simModelProvider );
    }
    removeActiveContextChangeListener( m_perspectiveWatcher );
    m_perspectiveWatcher.dispose();
    m_simModelProvider.dispose();
  }

  synchronized public void setActiveProject( final IProject activeProject )
  {
    if( m_activeProject == activeProject )
    {
      return;
    }
    logger.info( "New Project to Set:" + activeProject );
    try
    {
      if( Kalypso1D2DProjectNature.isOfThisNature( activeProject ) )
      {
        final Kalypso1D2DProjectNature nature = Kalypso1D2DProjectNature.toThisNature( activeProject );
        m_activeProject = activeProject;
        m_scenarioManager = nature.getScenarioManager();
        logger.info( "WorkflowDB=" + m_scenarioManager );
      }
      else
      {
        m_activeProject = null;
        m_scenarioManager = null;
        logger.warning( "Project to set is not of 1d2d nature" );
      }
    }
    catch( final CoreException e )
    {
      logger.log( Level.SEVERE, "Error setting current project", e );
      return;
    }
    finally
    {
      /* Set base scenario as current for the newly selected project */
      final Scenario baseScenario = m_scenarioManager == null ? null : m_scenarioManager.getRootScenarios().get( 0 );

      if( m_scenarioManager != null )
        m_scenarioManager.setCurrentScenario( baseScenario );
      fireActiveProjectChanged( m_activeProject, baseScenario );
    }
  }

  synchronized public IProject getCurrentProject( )
  {
    return m_activeProject;
  }

  public IScenarioManager getScenarioManager( )
  {
    return m_scenarioManager;
  }

  synchronized public Scenario getCurrentScenario( )
  {
    if( m_scenarioManager == null )
    {
      return null;
    }
    return m_scenarioManager.getCurrentScenario();
  }

  synchronized public IFolder getCurrentScenarioFolder( )
  {
    if( m_scenarioManager == null )
    {
      return null;
    }
    final Scenario activeScenario = m_scenarioManager.getCurrentScenario();
    return (m_activeProject == null || activeScenario == null) ? null : m_activeProject.getFolder( m_scenarioManager.getProjectPath( activeScenario ) );
  }

  synchronized public void addActiveContextChangeListener( IActiveContextChangeListener l )
  {
    logger.info( "Registering Active context change listener:" + l );
    if( l == null )
    {
      return;
    }
    else
    {
      if( activeProjectChangeListener.contains( l ) )
      {
        return;
      }
      else
      {
        activeProjectChangeListener.add( l );
      }
    }
  }

  synchronized public void removeActiveContextChangeListener( IActiveContextChangeListener l )
  {
    if( l == null )
    {
      return;
    }
    else
    {
      if( activeProjectChangeListener.contains( l ) )
      {
        activeProjectChangeListener.add( l );
      }
      else
      {
        // empty
      }
    }
  }

  final private void fireActiveProjectChanged( final IProject newProject, final Scenario scenario )
  {
    for( final IActiveContextChangeListener l : activeProjectChangeListener )
    {
      l.activeContextChanged( newProject, scenario );
    }
  }

  public void setCurrentSzenario( final Scenario scenario )
  {
    final Scenario currentScenario = m_scenarioManager.getCurrentScenario();
    if( currentScenario == null && scenario == null )
      return;
    else if( scenario != null && currentScenario != null && currentScenario.getURI().equals( scenario.getURI() ) )
    {
      return;
    }
    else
    {
      ensureProject( scenario );
      if( m_scenarioManager != null )
        m_scenarioManager.setCurrentScenario( scenario );
      fireActiveProjectChanged( m_activeProject, scenario );
    }
  }

  private void ensureProject( final Scenario scenario )
  {
    try
    {
      if( scenario == null )
        setActiveProject( null );
      else
      {
        final URI uri = new URI( scenario.getURI() );
        final String projectName = uri.getHost();
        final IProject project = ResourcesPlugin.getWorkspace().getRoot().getProject( projectName );
        if( project.exists() && project.isOpen() )
          setActiveProject( project );
      }
    }
    catch( final URISyntaxException e )
    {
      e.printStackTrace();
    }

  }

  /**
   * @see de.renew.workflow.connector.ITaskExecutionAuthority#canStopTask(de.renew.workflow.base.Task)
   */
  public boolean canStopTask( final Task task )
  {
    final SzenarioDataProvider dataProvider = (SzenarioDataProvider) m_simModelProvider.getCurrentState().get( ISzenarioSourceProvider.ACTIVE_SZENARIO_DATA_PROVIDER_NAME );
    //check if any model is dirty
    if( !dataProvider.isDirty() )
      return true;
    final Shell activeShell = PlatformUI.getWorkbench().getDisplay().getActiveShell();
    final MessageDialog confirmDialog = new MessageDialog( activeShell, "Ungespeicherte Änderungen", null, "Es gibt ungespeicherte Änderungen. Was möchten Sie tun?", MessageDialog.QUESTION, new String[] {
        "Speichern", "Verwerfen", "Abbrechen" }, 1 );
    final boolean result;
    final int decision = confirmDialog.open();
    if( decision == 0 )
    {
      try
      {
        final IRunnableWithProgress op = new IRunnableWithProgress()
        {
          public void run( final IProgressMonitor monitor ) throws InvocationTargetException
          {
            try
            {
              dataProvider.saveModel( monitor );
            }
            catch( final CoreException e )
            {
              throw new InvocationTargetException( e );
            }
          }
        };
        new ProgressMonitorDialog( activeShell ).run( true, true, op );
      }
      catch( final InvocationTargetException e )
      {
        final IStatus status = StatusUtilities.statusFromThrowable( e );
        ErrorDialog.openError( activeShell, "Fehler beim Speichern der Daten", "Es ist ein Fehler beim Speichern der Daten aufgetreten.", status );
        Kalypso1d2dProjectPlugin.getDefault().getLog().log( status );
      }
      catch( final InterruptedException e )
      {
        // TODO handle cancelation
      }
      result = true;
    }
    else if( decision == 1 )
    {
      // discard changes, reload model
      dataProvider.reloadModel();
      result = true;
    }
    else
    {
      result = false;
    }

    return result;
  }

  public Properties createProperties( )
  {
    final Properties properties = new Properties();
    final IProject activeProject = getCurrentProject();
    if( activeProject != null )
    {
      final String projectPath = activeProject.getName();
      properties.put( MEMENTO_PROJECT, projectPath );
    }
    final Scenario currentScenario = getCurrentScenario();
    if( currentScenario != null )
    {
      final String scenarioString = currentScenario.getURI();
      properties.put( MEMENTO_SCENARIO, scenarioString );
    }
    return properties;
  }
}
