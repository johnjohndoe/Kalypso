
package org.kalypso.afgui.model;

/**
 * The interface that all classes have to implement to become
 * a worfklow activity.
 * A workflow activity has :
 * 
 * @author Patrice Congo
 *
 */
public interface IActivity extends IWorkflowPart
{
	public String getURI();
}
