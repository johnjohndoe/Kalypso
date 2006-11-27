/**
 * 
 */
package org.kalypso.afgui.db;

/**
 * Define property type available in the worflow system
 * 
 * @author Patrice Congo
 *
 */
public enum EWorkflowProperty
{
	/**To define the type of work flow data*/
	HAS_TYPE,
	
	/**To define a derivation association between Data */
	IS_DERIVED_FROM,
	
	/** to dedines the location of worflow data*/
	HAS_LOCATION,
	
	/**To define the data a worflow part is working on */
	WORKS_ON,
	CONTAINS
	
	;
	
	
	
	
}
