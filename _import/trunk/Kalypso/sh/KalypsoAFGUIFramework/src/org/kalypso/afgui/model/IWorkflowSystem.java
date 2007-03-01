
package org.kalypso.afgui.model;

import org.kalypso.workflow.Workflow;


/**
 * Classes implements this interface to provide mechanism wot resolve
 * a Workflow associated to a resource of a particular kind
 * 
 * @author Patrice Congo
 */
public interface IWorkflowSystem
{
	public IWorkflow getWorflow(IWorkflowData dataModel);
	
	public Workflow getCurrentWorkFlow();
	
	public IWorkflow setCurrent(IWorkflowData dataModel);
	
	public void updateRTContext(
			IWorkflowPartRTContext currentWorkflowRTContext,
			IWorkflowPartRTContext currentTaskGroupRTContext,
			IWorkflowPartRTContext currentSubTaskGroupRTContext,
			IWorkflowPartRTContext currentTaskRTContext
			);
	public void updateRTContext(
			IWorkflowPartRTContext currentTaskGroupRTContext,
			IWorkflowPartRTContext currentSubTaskGroupRTContext,
			IWorkflowPartRTContext currentTaskRTContext
			);
	public void updateRTContext(
			IWorkflowPartRTContext currentSubTaskGroupRTContext,
			IWorkflowPartRTContext currentTaskRTContext
			);
	public void updateRTContext(
			IWorkflowPartRTContext currentTaskRTContext
			);
}
