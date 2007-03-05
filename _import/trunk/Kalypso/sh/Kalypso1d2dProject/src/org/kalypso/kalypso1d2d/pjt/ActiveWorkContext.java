package org.kalypso.kalypso1d2d.pjt;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Platform;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.afgui.db.IWorkflowDB;
import org.kalypso.afgui.model.IWorkflowSystem;
import org.kalypso.kalypso1d2d.pjt.actions.ProjectChangeListener;
import org.kalypso.kalypso1d2d.pjt.views.ISzenarioDataProvider;
import org.kalypso.kalypso1d2d.pjt.views.SzenarioDataProvider;
import org.kalypso.scenarios.Scenario;
import org.kalypso.workflow.Workflow;

//TODO move to workflow system problem with project??

/**
 * Represents the work context for a user. A workkontext is made of:
 * <ul>
 * <li/>The actuel project the user is working on <li/>The Workflow system <li/>The data basis system
 * </ul>
 * 
 * @author Patrice Congo
 */
public class ActiveWorkContext
{
  final static Logger logger = Logger.getLogger( ActiveWorkContext.class.getName() );

  private static final boolean log = Boolean.parseBoolean( Platform.getDebugOption( "org.kalypso.kalypso1d2d.pjt/debug" ) );

  static
  {
    if( !log )
      logger.setUseParentHandlers( false );
  }

  private final static ActiveWorkContext activeWorkContext = new ActiveWorkContext();

  private static final String BASIS_SCENARIO = "http://www.tu-harburg.de/wb/kalypso/kb/workflow/test/Basis";

  private Scenario m_activeScenario;

  private IWorkflowDB workflowDB;

  private IWorkflowSystem workflowSystem;

  private final SzenarioDataProvider m_dataProvider = new SzenarioDataProvider();

  private IProject m_activeProject;

  private List<IActiveContextChangeListener> activeProjectChangeListener = new ArrayList<IActiveContextChangeListener>();

  private ActiveWorkContext( )
  {
    final SzenarioSourceProvider simModelProvider = new SzenarioSourceProvider( this );
    final IHandlerService service = (IHandlerService) PlatformUI.getWorkbench().getService( IHandlerService.class );
    service.addSourceProvider( simModelProvider );
    final ProjectChangeListener projectChangeListener = new ProjectChangeListener();
    addActiveContextChangeListener( projectChangeListener );
    // TODO remove source provider and projectChangeListener somewhere
  }

  synchronized public void setActiveProject( final IProject activeProject )
  {
    if( this.m_activeProject == activeProject )
    {
      return;
    }
    logger.info( "New Project to Set:" + activeProject );
    final IWorkflowDB oldWorkflowDB = getWorkflowDB();
    try
    {
      if( oldWorkflowDB != null )
      {
        oldWorkflowDB.persist();
      }
      if( Kalypso1D2DProjectNature.isOfThisNature( activeProject ) )
      {
        final Kalypso1D2DProjectNature nature = Kalypso1D2DProjectNature.toThisNature( activeProject );
        this.m_activeProject = activeProject;
        this.workflowDB = nature.getWorkflowDB();
        this.workflowSystem = nature.getWorkflowSystem();
        logger.info( "WorkflowDB=" + workflowDB );
        logger.info( "WorkflowSystem:" + workflowSystem );
      }
      else
      {
        this.m_activeProject = null;
        this.workflowDB = null;
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
      fireActiveProjectChanged( activeProject, workflowDB.getScenario( BASIS_SCENARIO ) );
    }
  }

  final static public ActiveWorkContext getInstance( )
  {
    return activeWorkContext;
  }

  synchronized public IProject getActiveProject( )
  {
    return m_activeProject;
  }

  synchronized public Scenario getActiveScenario( )
  {
    return m_activeScenario;
  }

  synchronized public IWorkflowDB getWorkflowDB( )
  {
    return workflowDB;
  }

  synchronized public IWorkflowSystem getWorkflowSystem( )
  {
    return workflowSystem;
  }

  public Workflow getCurrentWorkflow( )
  {
    if( m_activeProject == null )
    {
      return null;
    }
    if( workflowSystem == null )
    {
      return null;
    }
    else
    {
      return workflowSystem.getCurrentWorkFlow();
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

  synchronized public void removeAllActiveContextChangeListener( )
  {
    activeProjectChangeListener.clear();
  }

  final private void fireActiveProjectChanged( final IProject newProject, final Scenario scenario )
  {
    for( final IActiveContextChangeListener l : activeProjectChangeListener )
    {
      l.activeContextChanged( newProject, scenario );
    }
  }

  public ISzenarioDataProvider getSzenarioDataProvider( )
  {
    return m_dataProvider;
  }

  public void setCurrentSzenario( final Scenario scenario )
  {
    final IProject activeProject = getActiveProject();
    m_activeScenario = scenario;
    m_dataProvider.setCurrent( activeProject, scenario );
    fireActiveProjectChanged( activeProject, scenario );
  }
}
