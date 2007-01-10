package org.kalypso.afgui.model;

import java.util.List;

/**
 * Represents a concrete piece of work for the user. A Task has a data model
 * associated to it and a working pannel.
 * 
 * @author Patrice Congo
 * 
 */
public interface ITaskGroup extends ITask {
	public List<ITaskGroup> getSubTasks();
	public List<ITask> getTasks();	
}
