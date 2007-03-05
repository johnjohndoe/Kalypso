
package org.kalypso.afgui.model;

import org.kalypso.workflow.Workflow;


/**
 * Classes implements this interface to provide mechanism wot resolve
 * a Workflow associated to a resource of a particular kind
 * 
 * @author Patrice Congo, Stefan Kurzbach
 */
public interface IWorkflowSystem
{
	public Workflow getCurrentWorkFlow();	
}
