
package org.kalypso.afgui.model;


/**
 * Classes implements this interface to provide mechanism wot resolve
 * a Workflow associated to a resource of a particular kind
 * 
 * @author Patrice Congo
 */
public interface IWorkflowSystem
{
	public IWorkflow getWorflow(IWorkflowData dataModel);
	
	public IWorkflow getCurrentWorkFlow();
	
	public IWorkflow setCurrent(IWorkflowData dataModel);
	
}
