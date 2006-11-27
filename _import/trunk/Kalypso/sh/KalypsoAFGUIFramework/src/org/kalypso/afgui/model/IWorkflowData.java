
package org.kalypso.afgui.model;

/**
 * Abtract the data model for a task or an activity.
 * 
 * @author Patrice Congo
 *
 */
public interface IWorkflowData
{
	public Object  getModelObject();
	public String getURI();
	public String getLocation();
	public String getType();
	
}
