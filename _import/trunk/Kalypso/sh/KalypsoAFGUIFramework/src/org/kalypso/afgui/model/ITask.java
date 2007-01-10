package org.kalypso.afgui.model;

import java.util.List;

/**
 * Represents a concrete piece of work for the user. A Task has a data model
 * associated to it and a working pannel.
 * 
 * @author Patrice Congo
 * 
 */
public interface ITask extends IWorkflowPart {
	/**
	 * returns the activities associated with this task
	 */
	public List<IActivity> getActivities();
}
