/**
 * 
 */
package org.kalypso.afgui.model;

/**
 * @author Patrice Congo
 *
 */
public interface IWorkflowCommand
{
//	/**
//	 * Sets the activity on which the action is executed
//	 * 
//	 * @param activity
//	 */
//	public void setActivity(IActivity activity);
//	
//	/**
//	 * Set the workflow on which the action is executed
//	 * @param workflow
//	 */
//	public void setWorkFlow(IWorkflow workflow);
	
//	/**
//	 * Sets the action key
//	 * @param action
//	 */
//	public void setAction(EActivityAction action);
	
	/**
	 * executes the command
	 */
	public void execute();
	/**
	 * The work flow for which this command
	 * will be executed
	 * @return
	 */
	public IWorkflow getWorkflow();
	
	/**
	 * Gets the action for which the command will be executed
	 * @return the action for which the command will be executed
	 */
	public IActivity getActivity();
	
	/**
	 * Get the activity key
	 * @return
	 */
	public EActivityAction getActivityAction();
}
