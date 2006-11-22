/**
 * 
 */
package org.kalypso.afgui.model;

import org.eclipse.draw2d.IFigure;

/**
 * Represents a phase in a Workflow. 
 * A phase is basicaly an activity container
 * 
 * @author Patrice Congo
 */
public interface IPhase
{
	/**
	 * Gets phase help 
	 * @return phase help resource as IHelp
	 */
	public IHelp getHelp();
	
	/**
	 * Gets the activities contain in this phase
	 * 
	 * @return  an array of IActivity 
	 */
	public IActivity[] getActivities();

	public String getName();
	
}
