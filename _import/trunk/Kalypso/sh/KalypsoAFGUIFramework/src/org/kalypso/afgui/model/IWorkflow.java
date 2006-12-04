package org.kalypso.afgui.model;

import java.util.List;

import org.kalypso.afgui.model.events.WorkflowChangeEvent;
import org.kalypso.afgui.model.events.WorkflowChangeEventListerner;

public interface IWorkflow extends IPhase
{

	
	
	
	
	
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
	


	public List<IPhase> getPhases();
	
	
}