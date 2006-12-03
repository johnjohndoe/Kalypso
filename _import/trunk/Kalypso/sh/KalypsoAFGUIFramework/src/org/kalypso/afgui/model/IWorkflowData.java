
package org.kalypso.afgui.model;

import java.util.List;

import org.kalypso.afgui.db.EWorkflowProperty;

/**
 * Abtract the data model for a task or an activity.
 * 
 * @author Patrice Congo
 *
 */
public interface IWorkflowData extends IWorkflowConcept
{
	public String getName();
	public Object  getModelObject();
	public String getURI();
	public String getLocation();
	public String getType();
	
	public  boolean hasLinkedWorkflowData(EWorkflowProperty prop);
	public List<IWorkflowData> getLinkedWorkflowData(EWorkflowProperty prop);
	
	
}
