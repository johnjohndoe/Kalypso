/**
 * 
 */
package org.kalypso.afgui.db;

import org.kalypso.afgui.model.IWorkflowData;

interface IWorkflowDataCreationMechanism
{
	public IWorkflowData create(
							Object model,
							String newID,
							String type,
							IWorkflowData parent);
}