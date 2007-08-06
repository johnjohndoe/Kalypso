/**
 * 
 */
package de.renew.workflow.connector.context;

import de.renew.workflow.cases.Case;
import de.renew.workflow.connector.cases.CaseHandlingProjectNature;

/**
 * Interface to implement in order to be notified if the active context changes as been changed
 * 
 * @author Patrice Congo
 */
public interface IActiveContextChangeListener<T extends Case>
{
  /* TODO: why do inform about the OLD worflow db and so on? Why not also the new one? */
  public void activeContextChanged( final CaseHandlingProjectNature newProject, final T scenario );
}
