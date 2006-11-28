/**
 * 
 */
package org.kalypso.kalypso1d2d.pjt;

import org.eclipse.core.resources.IProject;

/**
 * Interface to implement in order to be notified if the 
 * active project as been changed
 * 
 * @author Patrice Congo
 */
public interface IActiveProjectChangeListener
{
	public void activeProjectChanged(IProject newProject);
	
}
