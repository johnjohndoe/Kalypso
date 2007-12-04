package de.renew.workflow.connector.context;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.logging.Logger;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectNature;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;

import de.renew.workflow.cases.Case;
import de.renew.workflow.connector.WorkflowConnectorPlugin;
import de.renew.workflow.connector.cases.CaseHandlingProjectNature;
import de.renew.workflow.connector.cases.ICaseManager;

/**
 * Represents the work context for a user.
 * 
 * @author Stefan Kurzbach
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

  private CaseHandlingProjectNature m_currentProjectNature;

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
    workspace.addResourceChangeListener( this );
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
          final CaseHandlingProjectNature nature = (CaseHandlingProjectNature) project.getNature( m_natureID );
          if( nature == null )
          {
            final String message = String.format( "Cannot restore workflow state of project '%s'. It is not of nature: %s", projectString, m_natureID );
            throw new CoreException( new Status( IStatus.WARNING, WorkflowConnectorPlugin.PLUGIN_ID, message ) );
          }

          setCurrentProject( nature );

          final String caseId = properties.getProperty( MEMENTO_CASE );
          if( caseId != null )
          {
            final T caze = m_caseManager.getCase( caseId );
            setCurrentCase( caze );
          }
        }
        catch( final CoreException e )
        {
          log( e );
        }
      }
    }
  }

  private void log( final CoreException e )
  {
    log( e, "" );
  }

  private void log( final Exception e, final String message )
  {
    final IStatus status;
    if( e instanceof CoreException )
    {
      status = ((CoreException) e).getStatus();
    }
    else
    {
      status = new Status( Status.ERROR, WorkflowConnectorPlugin.PLUGIN_ID, message );
    }
    WorkflowConnectorPlugin.getDefault().getLog().log( status );
  }

  /**
   * Sets the active case handling project
   */
  @SuppressWarnings("unchecked")
  public void setCurrentProject( final CaseHandlingProjectNature currentProject ) throws CoreException
  {
    if( m_currentProjectNature == currentProject )
    {
      return;
    }

    if( currentProject != null )
    {
      m_currentProjectNature = currentProject;
      m_caseManager = currentProject.getCaseManager();

      /* Set base case as current for the newly selected project */
      final T caseToActivate = m_caseManager == null ? null : m_caseManager.getCases().get( 0 );

      if( m_caseManager != null )
        m_caseManager.setCurrentCase( caseToActivate );

      final CaseHandlingProjectNature currentProjectNature = m_currentProjectNature;
      PlatformUI.getWorkbench().getDisplay().asyncExec( new Runnable()
      {
        public void run( )
        {
          fireActiveContextChanged( currentProjectNature, caseToActivate );
        }
      } );
    }
    else
    {
      m_currentProjectNature = null;
      m_caseManager = null;
    }
  }

  public CaseHandlingProjectNature getCurrentProject( )
  {
    return m_currentProjectNature;
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
    m_activeContextChangeListeners.add( l );
  }

  public void removeActiveContextChangeListener( final IActiveContextChangeListener<T> l )
  {
    m_activeContextChangeListeners.remove( l );
  }

  protected void fireActiveContextChanged( final CaseHandlingProjectNature newProject, final T caze )
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
      final Job job = new Job( "Szenario aktivieren" )
      {
        /**
         * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.IProgressMonitor)
         */
        @SuppressWarnings("synthetic-access")
        @Override
        protected IStatus run( IProgressMonitor monitor )
        {
          try
          {
            ensureProject( caze );
          }
          catch( final CoreException e )
          {
            return e.getStatus();
          }
          if( m_caseManager != null )
            m_caseManager.setCurrentCase( caze );
          fireActiveContextChanged( m_currentProjectNature, caze );
          return Status.OK_STATUS;
        }
      };
      job.setRule( ResourcesPlugin.getWorkspace().getRoot() );
      job.schedule();
    }
  }

  /**
   * Sets the project to the project of the case if it has that information
   */
  private void ensureProject( final T caze ) throws CoreException
  {
    if( caze == null )
      setCurrentProject( null );
    else
    {
      final IProject project = getProject( caze );
      if( project.exists() )
      {
        // open a closed project, should we do this?
        project.open( null );
        setCurrentProject( (CaseHandlingProjectNature) project.getNature( m_natureID ) );
      }
      else
      {
        throw new CoreException( new Status( Status.ERROR, WorkflowConnectorPlugin.PLUGIN_ID, "Das Projekt " + project.getName() + " für den Case " + caze.getName() + " existiert nicht." ) );
      }
    }
  }

  private IProject getProject( final T caze ) throws CoreException
  {
    try
    {
      final URI uri = new URI( caze.getURI() );
      final IProject project = ResourcesPlugin.getWorkspace().getRoot().getProject( uri.getAuthority() );
      return project;
    }
    catch( final URISyntaxException e )
    {
      throw new CoreException( StatusUtilities.statusFromThrowable( e ) );
    }
  }

  /**
   * @see org.eclipse.core.resources.IResourceChangeListener#resourceChanged(org.eclipse.core.resources.IResourceChangeEvent)
   */
  public void resourceChanged( final IResourceChangeEvent event )
  {
    final IResourceDelta delta = event.getDelta();
    final Display display = PlatformUI.getWorkbench().getDisplay();
    if( (event.getType() == IResourceChangeEvent.PRE_DELETE || event.getType() == IResourceChangeEvent.PRE_CLOSE) )
    {
      if( m_currentProjectNature != null && m_currentProjectNature.getProject().equals( event.getResource() ) )
      {
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
    else if( event.getType() == IResourceChangeEvent.POST_CHANGE )
    {
      final IResourceDelta[] openedChildren = delta.getAffectedChildren();
      IResource resource = null;

      if( openedChildren.length > 0 )
      {
        final IResourceDelta resourceDelta = openedChildren[0];
        if( (resourceDelta.getFlags() & (IResourceDelta.OPEN | IResourceDelta.ADDED)) > 0 )
          resource = resourceDelta.getResource();
      }

      if( resource != null && resource.getType() == IResource.PROJECT )
      {
        final IProject project = (IProject) resource;
        if( project.isOpen() )
        {
          IProjectNature nature = null;
          try
          {
            nature = project.getNature( m_natureID );
          }
          catch( final CoreException e )
          {
            // nature does not exist or such, ignore
            e.printStackTrace();
          }
          if( nature != null )
          {
            final CaseHandlingProjectNature caseHandlingNature = (CaseHandlingProjectNature) nature;
            try
            {
              setCurrentProject( caseHandlingNature );
            }
            catch( final CoreException e )
            {
              final Shell activeShell = display.getActiveShell();
              final IStatus status = e.getStatus();
              ErrorDialog.openError( activeShell, "Problem beim Öffnen des Projektes", "Projekt wurde nicht aktiviert.", status );
              WorkflowConnectorPlugin.getDefault().getLog().log( status );
            }
          }
          else
          // nature is null
          {
            if( m_currentProjectNature != null && project.equals( m_currentProjectNature.getProject() ) )
            {
              try
              {
                setCurrentProject( null );
              }
              catch( final CoreException e )
              {
                final Shell activeShell = display.getActiveShell();
                final IStatus status = e.getStatus();
                ErrorDialog.openError( activeShell, "Problem", "Projekt wurde nicht deaktiviert.", status );
                WorkflowConnectorPlugin.getDefault().getLog().log( status );
              }
            }
          }
        }
      }
    }

  }
}
