/**
 * 
 */
package org.kalypso.afgui.model;

import java.util.List;

/**
 * @author Patrice Congo
 */
public interface ISubTaskGroup extends IWorkflowPart
{
	public List<ITask>getTasks();
}
