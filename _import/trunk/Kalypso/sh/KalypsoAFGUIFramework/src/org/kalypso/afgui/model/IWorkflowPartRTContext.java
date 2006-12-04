package org.kalypso.afgui.model;

public interface IWorkflowPartRTContext
{
	public IWorkflowPart getExecutingWorkflowPart();
	
	public IWorkflowData getProcessedWorkflowData();
	
	
	public EActivityExeState getExeState();
	
	public Object getModelObject();
	
}
