package de.renew.workflow.connector.context;

import java.io.UnsupportedEncodingException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URLDecoder;
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
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.ListDialog;
import org.eclipse.ui.model.WorkbenchContentProvider;
import org.eclipse.ui.model.WorkbenchLabelProvider;

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

  private final List<IActiveScenarioChangeListener<T>> m_activeContextChangeListeners = new ArrayList<IActiveScenarioChangeListener<T>>();

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

  public void addActiveContextChangeListener( final IActiveScenarioChangeListener<T> l )
  {
    m_activeContextChangeListeners.add( l );
  }

  public void removeActiveContextChangeListener( final IActiveScenarioChangeListener<T> l )
  {
    m_activeContextChangeListeners.remove( l );
  }

  protected void fireActiveContextChanged( final CaseHandlingProjectNature newProject, final T caze )
  {
    // Convert to array to avoid concurrent modification exceptions
    final IActiveScenarioChangeListener<T>[] listeners = m_activeContextChangeListeners.toArray( new IActiveScenarioChangeListener[m_activeContextChangeListeners.size()] );
    for( final IActiveScenarioChangeListener<T> l : listeners )
      l.activeScenarioChanged( newProject, caze );
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

      fireActiveContextChanged( m_currentProjectNature, caze );
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
        final String projectName = URLDecoder.decode( uri.getAuthority(), "UTF-8" );
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
    catch( UnsupportedEncodingException e )
    {
      e.printStackTrace();
    }
  }

  /**
   * @see org.eclipse.core.resources.IResourceChangeListener#resourceChanged(org.eclipse.core.resources.IResourceChangeEvent)
   */
  @SuppressWarnings("unchecked")
  public void resourceChanged( final IResourceChangeEvent event )
  {
    final IResourceDelta delta = event.getDelta();
    final Display display = PlatformUI.getWorkbench().getDisplay();
    if( (event.getType() == IResourceChangeEvent.PRE_DELETE || event.getType() == IResourceChangeEvent.PRE_CLOSE) )
    {
      // if the currently active project is deleted or closed
      if( m_currentProjectNature != null && m_currentProjectNature.getProject().equals( event.getResource() ) )
      {
        display.asyncExec( new Runnable()
        {
          /**
           * @see java.lang.Runnable#run()
           */
          public void run( )
          {
            try
            {
              // deactivate the current case
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
      // post change event after some modifications
      final IResourceDelta[] openedChildren = delta.getAffectedChildren();
      IResource resource = null;

      if( openedChildren.length > 0 )
      {
        final IResourceDelta resourceDelta = openedChildren[0];
        if( (resourceDelta.getFlags() & (IResourceDelta.OPEN | IResourceDelta.ADDED)) > 0 )
          // if the first affected resource was opened or added, remember this resource
          resource = resourceDelta.getResource();
      }

      if( resource != null && resource.getType() == IResource.PROJECT )
      {
        // this is an opened or added resource, if it is a project that was opened or added, go on
        final IProject project = (IProject) resource;
        if( project.isOpen() )
        {

          display.asyncExec( new Runnable()
          {
            /**
             * @see java.lang.Runnable#run()
             */
            public void run( )
            {
              IProjectNature nature = null;
              try
              {
                // get the case handling project nature if available
                nature = project.getNature( m_natureID );
              }
              catch( final CoreException e )
              {
                // nature does not exist or such, ignore
              }
              final Shell activeShell = display.getActiveShell();
              if( nature != null )
              {
                // if project has a nature
                final CaseHandlingProjectNature caseHandlingNature = (CaseHandlingProjectNature) nature;
                try
                {
                  final WorkbenchContentProvider workbenchContentProvider = new WorkbenchContentProvider();
                  final WorkbenchLabelProvider workbenchLabelProvider = new WorkbenchLabelProvider();
                  final ListDialog dialog = new ListDialog( activeShell );
                  dialog.setTitle( "Szenario-Projekt" );
                  dialog.setMessage( String.format( "Das Projekt \"%s\" enthält Szenarien.\nMöchten Sie ein Szenario aktivieren?", project.getName() ) );
                  dialog.setContentProvider( workbenchContentProvider );
                  dialog.setLabelProvider( workbenchLabelProvider );
                  dialog.setInput( caseHandlingNature );
                  dialog.setBlockOnOpen( true );

                  dialog.open();
                  final Object[] result = dialog.getResult();
                  if( result != null )
                  {
                    final T caze = (T) result[0];
                    setCurrentProject( caseHandlingNature );
                    setCurrentCase( caze );
                  }
                }
                catch( final CoreException e )
                {
                  final IStatus status = e.getStatus();
                  ErrorDialog.openError( activeShell, "Problem beim Öffnen des Projektes", "Projekt wurde nicht aktiviert.", status );
                  WorkflowConnectorPlugin.getDefault().getLog().log( status );
                }
              }
              else
              // nature is null, so it must have been removed from this project
              {
                if( m_currentProjectNature != null && project.equals( m_currentProjectNature.getProject() ) )
                {
                  try
                  {
                    setCurrentProject( null );
                  }
                  catch( final CoreException e )
                  {
                    final IStatus status = e.getStatus();
                    ErrorDialog.openError( activeShell, "Problem", "Projekt wurde nicht deaktiviert.", status );
                    WorkflowConnectorPlugin.getDefault().getLog().log( status );
                  }
                }
              }
            }
          } );
        }
      }
    }
  }
}
