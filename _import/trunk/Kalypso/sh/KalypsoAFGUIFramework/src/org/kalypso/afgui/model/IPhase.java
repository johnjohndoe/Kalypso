/**
 * 
 */
package org.kalypso.afgui.model;

import java.util.List;

/**
 * 
 * @author Patrice Congo
 */
public interface IPhase extends ITaskGroup
{
	List<ITaskGroup> getTaskGroups();
	
}
