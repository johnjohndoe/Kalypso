
package org.kalypso.afgui.model;

import java.util.List;

/**
 * The interface that all classes have to implement to become
 * a worfklow activity.
 * A workflow activity has :
 * <ul>
 * 	<li/>a specification which holds data specifying the activity. 
 * 		see {@link IActivitySpecification}for more information
 *  <li/>a current status which is a 
 * 
 * @author Patrice Congo
 *
 */
public interface IActivity
{
	
	/**
	 * To get the activity specification of this activity
	 * @return the actvity specification
	 * @see IActivitySpecification
	 */
	public IActivitySpecification getActivitySpecification();
	
	/**
	 * Returns the full runtime status of an activity.
	 * 
	 * @return the runtime status of the activity.
	 * @see IActivityRuntimeStatus 
	 */
	public IActivityRuntimeStatus getRuntimeStatus();
	
	/**
	 * A conveniance method to get the execution state of an activity.
	 * An activity can be started, ongoing or done.
	 * 
	 * Since the status in subject to changed the implementations of 
	 * {@link #getStatus()} and {@link #setStatus(EActivityExeState)}
	 * must cope with concurency access
	 *  
	 * @return the status of the activity as integer code:
	 * <ul>
	 * 	<li/>{@link EActivityExeState#NOT_STARTED} if the activity is not started jet
	 * 	<li/>{@link EActivityExeState#ON_GOING} if the activity is in processing
	 * 	<li/>{@link EActivityExeState#DONE} if the activity is done
	 * </ul>
	 */
	public EActivityExeState getExeState();
	
	/**
	 * To change the status of this activity.
	 * 
	 * Since the status in subject to changed theimplementations of 
	 * {@link #getStatus()} and {@link #setStatus(EActivityExeState)}
	 * must cope with concurency access
	 * 
	 * @param the new status
	 */
	public void setExeState(EActivityExeState status);
	
	/**
	 * Call to get the name of an activity
	 * @return the name of the activity
	 */
	public String getName();
	
	public String getID();
	
	public String getHelp();
	
	public ITask[] getTask();
	
	/**
	 * To retrieve the children activity of a given activity.
	 * The childen activity are linked to a parent activities through a part-of
	 * relationship.
	 * @param activity the activity on which the request is made
	 * @param aRelationship a relationship describing the link
	 * @throws IllegalArgumentException if either activity or relationship are null
	 * 
	 */
	public List<IActivity> getChildrenActivities(
									IActivity activity,
									EActivityRelationship relationship);
	
}
