/**
 * 
 */
package de.renew.workflow.base;

import java.util.logging.Logger;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ProjectScope;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.IEclipsePreferences.IPreferenceChangeListener;
import org.eclipse.core.runtime.preferences.IEclipsePreferences.PreferenceChangeEvent;

import de.renew.workflow.contexts.WorkflowSystemExtension;

/**
 * This workflow system manages the workflow instance in a description file in the project .metadata folder
 * 
 * @author Patrice Congo, Stefan Kurzbach
 */
public class WorkflowSystem implements IWorkflowSystem, IPreferenceChangeListener
{
  public static final String WORKFLOW = "de.renew.workflow.model"; //$NON-NLS-1$

  private static final String WORKFLOW_DEFINITON_ID = "workflowDefinition"; //$NON-NLS-1$

  private static final Logger logger = Logger.getLogger( WorkflowSystem.class.getName() );

  private static final boolean log = Boolean.parseBoolean( Platform.getDebugOption( "org.kalypso.afgui/debug" ) ); //$NON-NLS-1$

  static
  {
    if( !log )
      logger.setUseParentHandlers( false );
  }

  private Workflow m_currentWorkflow;

  private final IProject m_project;

  /**
   * Loads a workflow instance for the project
   * 
   * @exception CoreException
   *                if this method fails. Reasons include:
   *                <ul>
   *                <li> The metadata folder is not accessible.</li>
   *                <li> There is a problem loading the workflow.</li>
   */
  public WorkflowSystem( final IProject project ) throws CoreException
  {
    m_project = project;
    final ProjectScope projectScope = new ProjectScope( project );
    final IEclipsePreferences node = projectScope.getNode( WORKFLOW );
    node.addPreferenceChangeListener( this );
    final String workflowId = node.get( WORKFLOW_DEFINITON_ID, "workflow1d2d" );
    handleWorkflowIdChanged( workflowId );
  }

  private void handleWorkflowIdChanged( final String workflowId ) throws CoreException
  {
    final Workflow workflow = WorkflowSystemExtension.getWorkflow( workflowId );
    if( workflow != null )
    {
      m_currentWorkflow = workflow;
      m_project.refreshLocal( 0, new NullProgressMonitor() );
    }
    else
    {
      m_currentWorkflow = null;
      m_project.touch( new NullProgressMonitor() );
      final IStatus status = new Status( Status.ERROR, "de.renew.workflow.model", "Workflow definition " + workflowId + " could not be found for project" + m_project.getName() + "." );
      throw new CoreException( status );
    }
  }

  /**
   * @see org.kalypso.afgui.model.IWorkflowSystem#getCurrentWorkFlow()
   */
  public Workflow getCurrentWorkflow( )
  {
    return m_currentWorkflow;
  }

  /**
   * @see org.eclipse.core.runtime.preferences.IEclipsePreferences.IPreferenceChangeListener#preferenceChange(org.eclipse.core.runtime.preferences.IEclipsePreferences.PreferenceChangeEvent)
   */
  public void preferenceChange( final PreferenceChangeEvent event )
  {
    // This event is not generated when the settings file is manually edited,
    // however it is called when the project is deleted!!
    // When the workflowId is set to null, handleWorkflowIdChanged() will throw
    // an exception, but this is not desired if the project is deleted.

    // if( event.getKey().equals( WORKFLOW_DEFINITON_ID ) )
    // {
    // final String newWorkflowId = (String) event.getNewValue();
    // try
    // {
    // handleWorkflowIdChanged( newWorkflowId );
    // }
    // catch( final CoreException e )
    // {
    // StatusManager.getManager().handle( e.getStatus(), StatusManager.SHOW | StatusManager.LOG );
    // }
    // }
  }
}
