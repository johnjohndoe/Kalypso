package org.kalypso.afgui.model;

/**
 * An activity may have the following execution state:
 * <ul>
 * 	<li/>not started
 * 	<li/>ongoing
 *  <li/>done 
 * </ul>
 * This enumerate class define those status
 * 
 * @author Patrice Congo
 *
 */
public enum EActivityExeState
{
	/**
	 * Status to signal that an activity is not stated yet
	 */
	NOT_STARTED,
	
	/**
	 * Status code to signal that an activity processing is ongoing
	 */
	ON_GOING,
	
	/**
	 * Status to signal that an activity is done
	 */
	DONE, 
	
	UNKNOWN;
	
	
}
