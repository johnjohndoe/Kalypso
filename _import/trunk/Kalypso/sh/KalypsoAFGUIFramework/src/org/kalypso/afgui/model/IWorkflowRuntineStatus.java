package org.kalypso.afgui.model;

import java.util.List;

public interface IWorkflowRuntineStatus
{

	/**
	 * Get the activities, which specification is on the given activity 
	 * specifiacation list.
	 * 
	 * @param specifications a list of activity specifications
	 * @return
	 */
	public List<IActivity> getActivities(
			List<IActivitySpecification> specifications)
			throws IllegalArgumentException;

	/**
	 * @return the currentActivity
	 */
	public IActivity getCurrentActivity();

	/**
	 * @param currentActivity the currentActivity to set
	 */
	public void setCurrentActivity(IActivity currentActivity);
	
	public void setCurrentAction(EActivityAction action);
	public EActivityAction getCurrentAction();

}