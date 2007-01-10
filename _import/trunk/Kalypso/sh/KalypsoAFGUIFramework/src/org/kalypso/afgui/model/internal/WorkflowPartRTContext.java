package org.kalypso.afgui.model.internal;

import org.kalypso.afgui.db.Schema;
import org.kalypso.afgui.model.EActivityExeState;
import org.kalypso.afgui.model.IWorkflowData;
import org.kalypso.afgui.model.IWorkflowPart;
import org.kalypso.afgui.model.IWorkflowPartRTContext;

import com.hp.hpl.jena.rdf.model.Resource;

/**
 * 
 * @author Patrice Congo
 * 
 */
public class WorkflowPartRTContext implements IWorkflowPartRTContext {
	final private Resource resource;

	public WorkflowPartRTContext(Resource resource) {
		this.resource = resource;
	}

	/**
	 * @see org.kalypso.afgui.model.IWorkflowPartContext#getExeState()
	 */
	public EActivityExeState getExeState() {
		return Schema.getExeState(resource);
	}

	/**
	 * @see org.kalypso.afgui.model.IWorkflowPartContext#getProcessedWorkflowData()
	 */
	public IWorkflowData getProcessedWorkflowData() {
		return Schema.getProcessedWorkflowData(resource);
	}

	/**
	 * @see org.kalypso.afgui.model.IWorkflowPartContext#getExecutingWorkflowPart()
	 */
	public IWorkflowPart getExecutingWorkflowPart() {
		return Schema.getExecutingWorkflowPart(resource);
	}

	/**
	 * @see org.kalypso.afgui.model.IWorkflowPartContext#getModelObject()
	 */
	public Object getModelObject() {
		return resource;
	}

}
