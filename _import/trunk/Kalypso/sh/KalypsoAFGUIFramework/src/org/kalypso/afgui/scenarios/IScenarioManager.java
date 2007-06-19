/**
 * 
 */
package org.kalypso.afgui.scenarios;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;

import de.renew.workflow.connector.cases.ICaseManager;

/**
 * An {@link IScenarioManager} manages all the scenarios in a project. It provides metadata about the scenarios and
 * organizes the scenario model data in a database. One scenario is considered the current scenario.
 * 
 * @author Stefan Kurzbach
 */
public interface IScenarioManager extends ICaseManager<Scenario>
{

  /**
   * Creates a new scenario with the given name. It is derived from <code>parentScenario</code>. The scenario
   * metadata file and the database will be updated to reflect the change. The name must be unique within the context of
   * parentScenario. A notification will be sent to registered listeners that the scenarios have changed. If
   * <code>parentScenario</code> is <code>null</code>, a new root scenario is created.
   * 
   * @exception CoreException
   *              if this method fails. Reasons include:
   *              <ul>
   *              <li> A scenario with the given name already exists.</li>
   *              <li> The name of this scenario is not valid (according to <code>IWorkspace.validateName</code>).</li>
   *              <li> There is a problem persisting the database. See {@link #persist()} for details.</li>
   */
  public Scenario deriveScenario( final String name, final Scenario parentScenario ) throws CoreException;

  /**
   * Saves the changes in the scenario structure to the database.
   * 
   * @param monitor
   *          the progess monitor to report progress to or <code>null</code> if no progress reporting is desired
   * @exception CoreException
   *              if this method fails. Reasons include:
   *              <ul>
   *              <li> The database is not accessible or writable.</li>
   *              <li> An error specific to the kind of database has occured. It will be included in the cause of the
   *              exception.</li>
   */
  public void persist( final IProgressMonitor monitor ) throws CoreException;
}
