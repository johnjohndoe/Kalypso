package org.kalypso.afgui.model;

public interface IWorkflowPartRTContext
{
	public IWorkflowPart getExecutingWorkflowPart();
	
	public IWorkflowData getExecutedWorkflowData();
	
	
	public EActivityExeState getExeState();
	
	public Object getModelObject();
	
}
