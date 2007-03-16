package org.kalypso.kalypso1d2d.pjt;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Platform;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.afgui.scenarios.IScenarioManager;
import org.kalypso.afgui.scenarios.Scenario;
import org.kalypso.afgui.scenarios.ScenarioManager;
import org.kalypso.afgui.workflow.IWorkflowSystem;
import org.kalypso.afgui.workflow.Workflow;
import org.kalypso.kalypso1d2d.pjt.actions.PerspectiveWatcher;

/**
 * Represents the work context for a user. A work context is made of:
 * <ul>
 * <li/>The current project the user is working on <li/>The workflow system <li/>A scenario manager instance
 * </ul>
 * 
 * @author Patrice Congo, Stefan Kurzbach
 */
public class ActiveWorkContext
{
  private final static Logger logger = Logger.getLogger( ActiveWorkContext.class.getName() );

  private static final boolean log = Boolean.parseBoolean( Platform.getDebugOption( "org.kalypso.kalypso1d2d.pjt/debug" ) );

  static
  {
    if( !log )
      logger.setUseParentHandlers( false );
  }

  private static final String BASIS_SCENARIO = "http://www.tu-harburg.de/wb/kalypso/kb/workflow/test/Basis";

  private ScenarioManager m_scenarioManager;

  private IWorkflowSystem m_workflowSystem;

  private IProject m_activeProject;

  private final List<IActiveContextChangeListener> activeProjectChangeListener = new ArrayList<IActiveContextChangeListener>();

  /**
   * list of registries where we are registered as listeners <br>
   * used for clean dispose
   */
  private final List<Object> m_registries = new ArrayList<Object>();

  private final SzenarioSourceProvider m_simModelProvider;

  private final PerspectiveWatcher m_perspectiveWatcher = new PerspectiveWatcher();

  public ActiveWorkContext( )
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
        m_workflowSystem = nature.getWorkflowSystem();
        logger.info( "WorkflowDB=" + m_scenarioManager );
        logger.info( "WorkflowSystem:" + m_workflowSystem );
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
      /* Set base szenarion as current for the newly selected project */
      final Scenario baseScenario = m_scenarioManager == null ? null : m_scenarioManager.getScenario( BASIS_SCENARIO );

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

  public Workflow getCurrentWorkflow( )
  {
    if( m_workflowSystem == null )
    {
      return null;
    }
    else
    {
      return m_workflowSystem.getCurrentWorkflow();
    }
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
    // this fixes the bug mentioned below.
    if( currentScenario == null && scenario == null )
    {
      return;
    }
    else if( scenario != null && currentScenario != null && currentScenario.getURI().equals( scenario.getURI() ) )
    {
      return;
    }
    else
    {
      m_scenarioManager.setCurrentScenario( scenario );
      fireActiveProjectChanged( m_activeProject, scenario );
    }
  }
}
