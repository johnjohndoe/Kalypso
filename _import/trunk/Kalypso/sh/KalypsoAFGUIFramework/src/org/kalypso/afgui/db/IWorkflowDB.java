/**
 * 
 */
package org.kalypso.afgui.db;

import java.util.List;

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
	
	public List<IWorkflowData> getRootWorkflowDataByType(String type);
	
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
	
	public boolean persist();
	
	/**
	 * Return workflow data which cannot be resolved
	 * 
	 * @return
	 */
	public List<IWorkflowData> getUnresolvable();
	
	public void addWorkflowDBChangeListener(IWorkflowDBChangeListerner l);
	
	public void removeWorkflowDBChangeListener(IWorkflowDBChangeListerner l);
	
	public void removeAllWorkflowDBChangeListener();
	
}
