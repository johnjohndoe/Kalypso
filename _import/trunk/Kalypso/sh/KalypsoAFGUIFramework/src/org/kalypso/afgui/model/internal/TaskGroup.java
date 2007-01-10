package org.kalypso.afgui.model.internal;

import java.util.List;

import org.kalypso.afgui.db.Schema;
import org.kalypso.afgui.model.ITask;
import org.kalypso.afgui.model.ITaskGroup;

import com.hp.hpl.jena.rdf.model.Resource;

/**
 * @author Patrice Congo
 * 
 */
public class TaskGroup extends Task implements ITaskGroup {

	public TaskGroup(Resource resource) {
		super(resource);
	}

	/**
	 * @see org.kalypso.afgui.model.ITask#getActivities()
	 */
	public List<ITaskGroup> getSubTasks() {
		return Schema.getTaskGroups(resource);
	}

	/**
	 * @see org.kalypso.afgui.model.ITask#getActivities()
	 */
	public List<ITask> getTasks() {
		return Schema.getTasks(resource);
	}

	@Override
	public String toString() {
		StringBuffer buffer = new StringBuffer("TaskGroup: ");
		buffer.append(getName());
		return buffer.toString();
	}
}
