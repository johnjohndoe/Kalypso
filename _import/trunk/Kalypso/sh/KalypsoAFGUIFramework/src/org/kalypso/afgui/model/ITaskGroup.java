package org.kalypso.afgui.model;

import java.util.List;


/**
 * @author Patrice Congo
 *
 */
public interface ITaskGroup extends ISubTaskGroup
{
	public List<ISubTaskGroup> getSubTaskGroups();
}
