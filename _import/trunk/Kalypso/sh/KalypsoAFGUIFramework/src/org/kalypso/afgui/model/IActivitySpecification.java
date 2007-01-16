/**
 * 
 */
package org.kalypso.afgui.model;

import java.util.List;

/**
 * The Interface to be implemented by classes which provides the 
 * specification of an activity:
 * <ul>
 *   <li>The mechanism to computer the activity data model
 *   	<li/>A specification of the supporing work panel which is used 
 *   	to fullfill the activity.
 *   	<li/>The Specification of assertion that should be made before accepting the 
 *   	data before accepting the activity as done. 
 *   </ul>
 *  
 * @author Patrice Congo
 *
 */
public interface IActivitySpecification
{
	/**
	 * To get the data model this activitz in working on
	 *  
	 * @return the data model for this activity
	 */
	public IWorkflowData getDataModel();
	
	/**
	 * To get the class of the work pannel class. The class of the 
	 * visual tool which provides the interface to do the activity 
	 *  
	 * @return the class of the visual component which provides the
	 * 
	 */
	public Class getWorkPannel();
	
	//TODO does it ges here
	public List<Object> getAssertions();
	
	/**
	 * To get the help text associated with the specified activity.
	 * the h
	 * 
	 * 
	 * @return
	 */
	public IActivityHelp getHelp();
	
	/**
	 * To get the description for an activity.
	 * 
	 * @return the activity description
	 */
	public String getDescription();
	
	/**
	 * Call to get the name of the specified activity
	 * An activity muss have a name. Therefore null is never return.
	 * The name of an Activity is also required to be unique within
	 * the scope of a workspace
	 *  
	 * @return the name of the activity
	 */
	public String getName();
	
	public String getID();
	
	public boolean isRoot();

}
