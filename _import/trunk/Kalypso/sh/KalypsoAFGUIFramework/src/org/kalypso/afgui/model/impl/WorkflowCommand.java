/**
 * 
 */
package org.kalypso.afgui.model.impl;

import org.kalypso.afgui.model.EActivityAction;
import org.kalypso.afgui.model.IActivity;
import org.kalypso.afgui.model.IWorkflow;
import org.kalypso.afgui.model.IWorkflowCommand;

/**
 * @author Patrice Congo
 *
 */
public class WorkflowCommand implements IWorkflowCommand
{
	final private IActivity activity;
	final private IWorkflow workflow;
	final private EActivityAction activityAction;
	
	public WorkflowCommand(
					IActivity activity,
					IWorkflow workflow,
					EActivityAction activityAction)
	{
		if(workflow==null)
		{
			throw new IllegalArgumentException(
					"Workflow must not be null");
		}
		this.workflow=workflow;
		this.activity=activity;
		this.activityAction=activityAction;
	}
	
	/* (non-Javadoc)
	 * @see org.kalypso.afgui.model.IWorkflowCommand#execute()
	 */
	public void execute()
	{
			workflow.updateWorkflow(activity, activityAction);
	}

	/* (non-Javadoc)
	 * @see org.kalypso.afgui.model.IWorkflowCommand#getActivity()
	 */
	public IActivity getActivity()
	{
		return activity;
	}

	/* (non-Javadoc)
	 * @see org.kalypso.afgui.model.IWorkflowCommand#getActivityAction()
	 */
	public EActivityAction getActivityAction()
	{
		return activityAction;
	}

	/* (non-Javadoc)
	 * @see org.kalypso.afgui.model.IWorkflowCommand#getWorkflow()
	 */
	public IWorkflow getWorkflow()
	{
		return workflow;
	}

}
