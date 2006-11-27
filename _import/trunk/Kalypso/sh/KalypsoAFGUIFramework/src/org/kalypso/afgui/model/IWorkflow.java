package org.kalypso.afgui.model;

import java.util.List;

import org.kalypso.afgui.model.events.WorkflowChangeEvent;
import org.kalypso.afgui.model.events.WorkflowChangeEventListerner;

public interface IWorkflow extends IActivity
{

	
	
	/**
	 * Returns the roots activity
	 * 
	 * @return
	 */
	public List<IActivity> getRootActivities();
	
	/**
	 * Return the runtime status of a workflow.
	 * @see IWorkflowRuntineStatus
	 * @return the runtime status of the workflow
	 */
	public IWorkflowRuntineStatus getRuntineStatus();
	
	/**
	 * Get the workflow specification
	 * 
	 * @return the workflow specification
	 */
	public IWorkflowSpecification getWorkflowSpecification();
	
	/**
	 * Gets the current activity the user is currently doing.
	 * 
	 * @return the current activity if the workflow as 
	 * 			been started or nulll
	 */
	public IActivity getCurrentActivity();
	
	/**
	 * Add a workflow changed listener to receive changed events.
	 * If the passed listener is already registered the action is
	 * skipped
	 * @see WorkflowChangeEventListerner
	 * @see WorkflowChangeEvent
	 * @param l -- the listner to register
	 * @throws IllegalArgumentException if l is null
	 */
	public void addWorkflowChangedEventListener(
								WorkflowChangeEventListerner l);
	/**
	 * Remove the specified worklfow change listener.
	 *  
	 * @param l -- the listener to remove from the listener list.
	 */
	public void removeWorkflowChangedEventListener(
								WorkflowChangeEventListerner l);
	
//	public void setAction(EActivityAction activityAction);
//	
//	public void setActiveAtivity(IActivity activity);
	
	/**
	 * Update the workflow considering the provited activity and the 
	 * request action.
	 * Valid action are define by {@link EActivityAction}
	 * 
	 * @param activity the activity concerned by the action
	 * @param action the action the workflow schould carry out
	 */
	void updateWorkflow(IActivity activity, EActivityAction action);
	
	/**
	 * Get this workflow adopt the status and specification of the specified
	 * workflow.
	 *  
	 * @param workflow -- the workflow that provides the status and 
	 * 			specification to adopt
	 */
	public void adopt(IWorkflow workflow);

	public IPhase[] getPhases();
	
	public Object getWrappedModel();
	
	public IWorkflowData getWorkflowDataModel();
	
}