package org.kalypso.afgui.model.impl;

import java.util.List;

import org.kalypso.afgui.model.IActivity;
import org.kalypso.afgui.model.ISubTaskGroup;
import org.kalypso.afgui.model.ITask;
import org.kalypso.afgui.model.ITaskGroup;
import org.kalypso.afgui.schema.Schema;

import com.hp.hpl.jena.rdf.model.Resource;

/**
 * @author Patrice Congo
 */
public class TaskGroup extends WorkflowPart implements ITaskGroup
{
	
	public TaskGroup(Resource resource)
	{
		super(resource);
	}
	
	/**
	 * @see org.kalypso.afgui.model.ITaskGroup#getSubTaskGroups()
	 */
	public List<ISubTaskGroup> getSubTaskGroups()
	{
		return Schema.getSubTaskGroups(resource);
	}

	/**
	 * @see org.kalypso.afgui.model.ISubTaskGroup#getTasks()
	 */
	public List<ITask> getTasks()
	{
		return Schema.getTasks(resource);
	}

	/**
	 * @see org.kalypso.afgui.model.ITask#getActivities()
	 */
	public List<IActivity> getActivities()
	{
		return Schema.getActivities(resource);
	}

}
