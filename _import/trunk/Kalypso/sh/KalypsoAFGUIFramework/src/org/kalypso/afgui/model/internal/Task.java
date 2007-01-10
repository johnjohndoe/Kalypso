package org.kalypso.afgui.model.internal;

import java.util.List;

import org.kalypso.afgui.db.Schema;
import org.kalypso.afgui.model.IActivity;
import org.kalypso.afgui.model.ITask;

import com.hp.hpl.jena.rdf.model.Resource;

/**
 * @author Patrice Congo
 * 
 */
public class Task extends WorkflowPart implements ITask {

	public Task(Resource resource) {
		super(resource);
	}

	/**
	 * @see org.kalypso.afgui.model.ITask#getActivities()
	 */
	public List<IActivity> getActivities() {
		return Schema.getActivities(resource);
	}

	@Override
	public String toString() {
		StringBuffer buffer = new StringBuffer("Task: ");
		buffer.append(getName());
		return buffer.toString();
	}
}
