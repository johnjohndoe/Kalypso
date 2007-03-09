package org.kalypso.afgui.workflow;


/**
 * A workflow system has one running workflow instance.
 * 
 * @author Patrice Congo, Stefan Kurzbach
 */
public interface IWorkflowSystem
{
  /**
   * Returns the workflow instance or <code>null</code> if no workflow instance has been created
   */
  public Workflow getCurrentWorkflow( );
}
