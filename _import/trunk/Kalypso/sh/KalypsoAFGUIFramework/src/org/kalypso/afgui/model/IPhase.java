/**
 * 
 */
package org.kalypso.afgui.model;

import java.util.List;

/**
 * @author Patrice Congo
 */
public interface IPhase extends IWorkflowPart
{
  public List<ITaskGroup> getTaskGroups( );
}
