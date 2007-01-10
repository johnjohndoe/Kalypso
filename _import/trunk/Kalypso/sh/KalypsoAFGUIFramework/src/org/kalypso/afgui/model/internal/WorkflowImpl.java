package org.kalypso.afgui.model.internal;

import java.util.List;

import org.kalypso.afgui.db.Schema;
import org.kalypso.afgui.model.ITaskGroup;
import org.kalypso.afgui.model.IWorkflow;

import com.hp.hpl.jena.rdf.model.Resource;

/**
 * This class abtract the workfow. a workflow is collection of activities.
 * In a workflow activities can be linked through:
 * <ul>
 * 	<li/>a has-a relationship. 
 * 		That is activities may be group in a container activity
 *  <li/>excecution dependency. 
 *  	That is a particular activity can only started if another is done.  
 * <ul/>
 * 
 * The work flow has a root activity, which represent the starting point of for the workflow.
 * The workflow provide the mechanism to navigate throw the the workflow.
 * Particulaly it provides an update() method, 
 * which can be used to update the state of the of the workflow.
 * E.g. transitiing to another activity.
 * 
 *  The Workflow also implements a Event 
 * @author Patrice Congo
 *
 */
/**
 * @author congo
 * 
 */
public class WorkflowImpl extends WorkflowPart implements IWorkflow {

	/**
	 * Construct a new Workflow specification based on the given workflow
	 * specification
	 * 
	 * @param workflowSpecification
	 *            the work flow specification this workflow is based on. this
	 *            parameter must not be null
	 * @param runtimeStatus
	 *            the runtime status for this workflow
	 * @throws IllegalArgumentException
	 *             if the parameter workflowSpecification is null
	 * 
	 */
	public WorkflowImpl(Resource resource) {
		super(resource);
	}

	@Override
	public String toString() {
		StringBuffer buffer = new StringBuffer("Workflow.");
		buffer.append(getName());
		return buffer.toString();
	}

	/**
	 * @see org.kalypso.afgui.model.ISubTaskGroup#getTasks()
	 */
	public List<ITaskGroup> getTaskGroups() {
		return Schema.getTaskGroups(resource);
	}
}
