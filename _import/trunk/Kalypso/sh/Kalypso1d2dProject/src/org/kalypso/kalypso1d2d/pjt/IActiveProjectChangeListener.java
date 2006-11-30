/**
 * 
 */
package org.kalypso.kalypso1d2d.pjt;

import org.eclipse.core.resources.IProject;
import org.kalypso.afgui.db.IWorkflowDB;
import org.kalypso.afgui.model.IWorkflowSystem;

/**
 * Interface to implement in order to be notified if the 
 * active project as been changed
 * 
 * @author Patrice Congo
 */
public interface IActiveProjectChangeListener
{
	public void activeProjectChanged(
					IProject newProject, 
					IProject oldProject,
					IWorkflowDB oldDB,
					IWorkflowSystem oldWorkflowSystem);
	
}
