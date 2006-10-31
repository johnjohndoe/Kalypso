package org.kalypso.afgui.model.events;

import java.util.EventObject;

import org.kalypso.afgui.model.IWorkflow;

/**
 * The event object use to notify changes in the current workflow model state.
 * 
 * @author Patrice Congo
 */
public class WorkflowChangeEvent extends EventObject
{
	
	public WorkflowChangeEvent(IWorkflow workflow)
	{
		super(workflow);
	}
}
