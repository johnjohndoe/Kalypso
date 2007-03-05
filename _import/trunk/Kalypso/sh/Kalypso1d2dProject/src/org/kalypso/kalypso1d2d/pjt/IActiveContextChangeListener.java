/**
 * 
 */
package org.kalypso.kalypso1d2d.pjt;

import org.eclipse.core.resources.IProject;
import org.kalypso.scenarios.Scenario;

/**
 * Interface to implement in order to be notified if the active context changes as been changed
 * 
 * @author Patrice Congo
 */
public interface IActiveContextChangeListener
{
  /* TODO: why do inform about the OLD worflow db and so on? Why not also the new one? */
  public void activeContextChanged( final IProject newProject, final Scenario scenario);
}
