/**
 * 
 */
package org.kalypso.afgui.db;

import java.net.URI;
import java.util.List;

import javax.management.relation.Relation;

import org.kalypso.afgui.model.IWorkflowData;

/**
 * Represents the workflow data base
 * 
 * @author Patrice Congo
 *
 */
public interface IWorkflowDB
{
	
	public List<IWorkflowData> getWorkflowDataByType(String type);
	
	public IWorkflowData getWorkflowDataById(String id);
	
	public IWorkflowData createWorkflowData(
										String id, 
										String type,
										IWorkflowData parent);
	
	public IWorkflowData derivedWorkflowData(
								IWorkflowData parent, 
								String childId);
	
	public void unlink(
				IWorkflowData subject, 
				IWorkflowData object, 
				EWorkflowProperty linkType);
	
	public void link(
				IWorkflowData subject, 
				IWorkflowData object, 
				EWorkflowProperty linkType);
	
	/**
	 * Return workflow data which cannot be resolved
	 * 
	 * @return
	 */
	public List<IWorkflowData> getUnresolvable();
	
}
