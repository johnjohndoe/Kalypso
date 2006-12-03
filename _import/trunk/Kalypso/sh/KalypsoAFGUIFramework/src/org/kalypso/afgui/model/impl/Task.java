package org.kalypso.afgui.model.impl;

import java.util.List;

import org.kalypso.afgui.model.IActivity;
import org.kalypso.afgui.model.ITask;
import org.kalypso.afgui.schema.Schema;

import com.hp.hpl.jena.rdf.model.Resource;

/**
 * @author Patrice Congo
 *
 */
public class Task extends WorkflowPart implements ITask
{

	public Task(Resource resource)
	{
		super(resource);
	}
	
	/**
	 * @see org.kalypso.afgui.model.ITask#getActivities()
	 */
	public List<IActivity> getActivities()
	{
		return Schema.getActivities(resource);
	}

}
