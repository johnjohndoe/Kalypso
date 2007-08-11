/**
 * 
 */
package de.renew.workflow.base;

import java.util.logging.Logger;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ProjectScope;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;

import de.renew.workflow.contexts.WorkflowSystemExtension;

/**
 * This workflow system manages the workflow instance in a description file in the project .metadata folder
 * 
 * @author Patrice Congo, Stefan Kurzbach
 */
public class WorkflowSystem implements IWorkflowSystem
{
  public static final String WORKFLOW = "de.renew.workflow.model";

  private static final String WORKFLOW_DEFINITON_ID = "workflowDefinition";

  private static final Logger logger = Logger.getLogger( WorkflowSystem.class.getName() );

  private static final boolean log = Boolean.parseBoolean( Platform.getDebugOption( "org.kalypso.afgui/debug" ) );

  static
  {
    if( !log )
      logger.setUseParentHandlers( false );
  }

  private Workflow m_currentWorkflow;

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
    final ProjectScope projectScope = new ProjectScope( project );
    final String workflowId = projectScope.getNode( WORKFLOW ).get( WORKFLOW_DEFINITON_ID, "workflow1d2d" );
    final Workflow workflow = WorkflowSystemExtension.getWorkflow( workflowId );
    if( workflow != null )
    {
      m_currentWorkflow = workflow;
    }
    else
    {
      final IStatus status = new Status( Status.ERROR, "de.renew.workflow.model", "Workflow definition could not be found." );
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
}
