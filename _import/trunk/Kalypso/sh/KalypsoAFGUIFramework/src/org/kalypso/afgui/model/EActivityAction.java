/**
 * 
 */
package org.kalypso.afgui.model;

/**
 * Action the user can invoque on an activity
 * 
 * @author Patrice Congo
 * 
 */
public enum EActivityAction {
	/**
	 * Do the activity
	 */
	DO,
	/**
	 * Get help for an activity
	 */
	GET_HELP,
	/**
	 * Get next possible activities
	 */
	NEXT,
	/**
	 * Get previous activities
	 */
	PREVIOUS,
	/**
	 * Go up the activity level
	 */
	UP,
	/**
	 * Go down the activity level
	 */
	DOWN,
	/**
	 * ShowActibity Data
	 */
	SHOW_DATA,
	/**
	 * Show Root Activity
	 */
	TOP,

	RELOAD;

}
