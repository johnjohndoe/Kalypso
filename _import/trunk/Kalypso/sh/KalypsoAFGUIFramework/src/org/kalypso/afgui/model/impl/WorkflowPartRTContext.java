package org.kalypso.afgui.model.impl;

import org.kalypso.afgui.model.EActivityExeState;
import org.kalypso.afgui.model.IWorkflowData;
import org.kalypso.afgui.model.IWorkflowPart;
import org.kalypso.afgui.model.IWorkflowPartRTContext;
import org.kalypso.afgui.schema.Schema;

import com.hp.hpl.jena.rdf.model.Resource;

/**
 * 
 * @author Patrice Congo
 *
 */
public class WorkflowPartRTContext implements IWorkflowPartRTContext
{
	final private Resource resource;
	
	public WorkflowPartRTContext(Resource resource)
	{
		this.resource=resource;
	}
	
	/* (non-Javadoc)
	 * @see org.kalypso.afgui.model.IWorkflowPartContext#getExeState()
	 */
	public EActivityExeState getExeState()
	{
		return Schema.getExeState(resource);
	}

	/* (non-Javadoc)
	 * @see org.kalypso.afgui.model.IWorkflowPartContext#getExecutedWorkflowData()
	 */
	public IWorkflowData getExecutedWorkflowData()
	{
		return Schema.getProcessedWorkflowData(resource);
	}

	/* (non-Javadoc)
	 * @see org.kalypso.afgui.model.IWorkflowPartContext#getExecutingWorkflowPart()
	 */
	public IWorkflowPart getExecutingWorkflowPart()
	{
		return Schema.getExecutingWorkflowPart(resource);
	}

	/* (non-Javadoc)
	 * @see org.kalypso.afgui.model.IWorkflowPartContext#getModelObject()
	 */
	public Object getModelObject()
	{
		return resource;
	}

}
