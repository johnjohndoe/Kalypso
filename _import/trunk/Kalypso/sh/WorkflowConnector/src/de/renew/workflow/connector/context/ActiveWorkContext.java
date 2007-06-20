package de.renew.workflow.connector.context;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.logging.Logger;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.ErrorSupportProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;

import de.renew.workflow.base.Workflow;
import de.renew.workflow.cases.Case;
import de.renew.workflow.connector.WorkflowConnectorPlugin;
import de.renew.workflow.connector.cases.ICaseManager;
import de.renew.workflow.connector.worklist.TaskExecutionListener;

/**
 * Represents the work context for a user.
 * 
 * @author Patrice Congo, Stefan Kurzbach
 */
public class ActiveWorkContext<T extends Case> implements IResourceChangeListener
{
  private final static Logger logger = Logger.getLogger( ActiveWorkContext.class.getName() );

  private static final boolean log = Boolean.parseBoolean( Platform.getDebugOption( "org.kalypso.kalypso1d2d.pjt/debug" ) );

  static
  {
    if( !log )
      logger.setUseParentHandlers( false );
  }

  public static final String MEMENTO_PROJECT = "project";

  public static final String MEMENTO_CASE = "case";

  private ICaseManager<T> m_caseManager;

  private CaseHandlingProjectNature m_currentProject;

  private final List<IActiveContextChangeListener<T>> m_activeContextChangeListeners = new ArrayList<IActiveContextChangeListener<T>>();

  private final String m_natureID;

  /**
   * Creates a new work context and restores the previous state from the given properties
   */
  public ActiveWorkContext( final Properties properties, final String natureID )
  {
    m_natureID = natureID;
    restoreState( properties );

    final IWorkspace workspace = ResourcesPlugin.getWorkspace();
    workspace.addResourceChangeListener( this, IResourceChangeEvent.PRE_DELETE );
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
        try
        {
          setCurrentProject( (CaseHandlingProjectNature) project.getNature( m_natureID ) );
        }
        catch( final CoreException e )
        {
          log( e );
        }

        final String caseId = properties.getProperty( MEMENTO_CASE );
        if( caseId != null )
        {
          final T caze = m_caseManager.getCase( caseId );
          try
          {
            setCurrentCase( caze );
          }
          catch( final CoreException e )
          {
            log( e );
          }
        }
      }
    }
  }

  private void log( final CoreException e )
  {
    WorkflowConnectorPlugin.getDefault().getLog().log( e.getStatus() );
  }

  /**
   * Sets the active case handling project
   */
  @SuppressWarnings("unchecked")
  public void setCurrentProject( final CaseHandlingProjectNature currentProject ) throws CoreException
  {
    if( m_currentProject == currentProject )
    {
      return;
    }

    if( currentProject != null )
    {
      m_currentProject = currentProject;
      m_caseManager = currentProject.getCaseManager();
    }
    else
    {
      m_currentProject = null;
      m_caseManager = null;
    }

    /* Set base case as current for the newly selected project */
    final T caseToActivate = m_caseManager == null ? null : m_caseManager.getCases().get( 0 );

    if( m_caseManager != null )
      m_caseManager.setCurrentCase( caseToActivate );
    fireActiveContextChanged( m_currentProject, caseToActivate );
  }

  public CaseHandlingProjectNature getCurrentProject( )
  {
    return m_currentProject;
  }

  /**
   * The same as {@link #getCaseManager()#getCurrentCase()}
   */
  public T getCurrentCase( )
  {
    if( m_caseManager == null )
    {
      return null;
    }
    return m_caseManager.getCurrentCase();
  }

  public void addActiveContextChangeListener( final IActiveContextChangeListener<T> l )
  {
    if( l == null )
    {
      return;
    }
    else
    {
      if( m_activeContextChangeListeners.contains( l ) )
      {
        return;
      }
      else
      {
        m_activeContextChangeListeners.add( l );
      }
    }
  }

  public void removeActiveContextChangeListener( final IActiveContextChangeListener<T> l )
  {
    if( l == null )
    {
      return;
    }
    else
    {
      if( m_activeContextChangeListeners.contains( l ) )
      {
        m_activeContextChangeListeners.add( l );
      }
      else
      {
        // empty
      }
    }
  }

  private void fireActiveContextChanged( final CaseHandlingProjectNature newProject, final T caze )
  {
    for( final IActiveContextChangeListener<T> l : m_activeContextChangeListeners )
    {
      l.activeContextChanged( newProject, caze );
    }
  }

  public void setCurrentCase( final T caze ) throws CoreException
  {
    final T currentCase;
    if( m_caseManager == null )
      currentCase = null;
    else
      currentCase = m_caseManager.getCurrentCase();
    if( currentCase == null && caze == null )
      return;
    else if( caze != null && currentCase != null && currentCase.getURI().equals( caze.getURI() ) )
    {
      return;
    }
    else
    {
      ensureProject( caze );
      if( m_caseManager != null )
        m_caseManager.setCurrentCase( caze );
      fireActiveContextChanged( m_currentProject, caze );
    }
  }

  /**
   * Sets the project to the project of the case if it has that information
   */
  private void ensureProject( final T caze ) throws CoreException
  {
    try
    {
      if( caze == null )
        setCurrentProject( null );
      else
      {
        final URI uri = new URI( caze.getURI() );
        final String projectName = uri.getHost();
        final IProject project = ResourcesPlugin.getWorkspace().getRoot().getProject( projectName );
        if( project.exists() )
        {
          // open a closed project, should we do this?
          project.open( null );
          setCurrentProject( (CaseHandlingProjectNature) project.getNature( m_natureID ) );
        }
        else
        {
          throw new CoreException( new Status( Status.ERROR, WorkflowConnectorPlugin.PLUGIN_ID, "Das Projekt " + projectName + " für den Case " + caze.getName() + " existiert nicht." ) );
        }
      }
    }
    catch( final URISyntaxException e )
    {
      e.printStackTrace();
    }
  }

  public Workflow getCurrentWorklist( )
  {
    return m_caseManager.getCurrentWorklist();
  }

  /**
   * @see org.eclipse.core.resources.IResourceChangeListener#resourceChanged(org.eclipse.core.resources.IResourceChangeEvent)
   */
  public void resourceChanged( final IResourceChangeEvent event )
  {
    if( event.getResource().equals( m_currentProject.getProject() ) )
    {
      final Display display = PlatformUI.getWorkbench().getDisplay();
      display.asyncExec( new Runnable()
      {

        public void run( )
        {
          try
          {
            setCurrentCase( null );
          }
          catch( final CoreException e )
          {
            final Shell activeShell = display.getActiveShell();
            final IStatus status = e.getStatus();
            ErrorDialog.openError( activeShell, "Problem beim Löschen des Projektes", "Projekt wurde nicht deaktiviert.", status );
            WorkflowConnectorPlugin.getDefault().getLog().log( status );
          }
        }
      } );
    }

  }
}
