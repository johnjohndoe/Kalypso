/**
 * 
 */
package org.kalypso.afgui.model;

import java.util.List;

/**
 * @author Patrice Congo
 */
public interface ISubTaskGroup extends ITask
{
	public List<ITask>getTasks();
}
