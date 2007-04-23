/**
 * 
 */
package org.kalypso.afgui.scenarios;

import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;

import de.renew.workflow.base.Workflow;

/**
 * An {@link IScenarioManager} manages all the scenarios in a project. It provides metadata about the scenarios and
 * organizes the scenario model data in a database. One scenario is considered the current scenario.
 * 
 * @author Stefan Kurzbach
 */
public interface IScenarioManager
{
  /**
   * Returns the current scenario
   */
  public Scenario getCurrentScenario( );

  /**
   * Returns the workflow for the current scenario
   */
  public Workflow getCurrentWorkflow( );

  /**
   * Sets the current scenario
   */
  public void setCurrentScenario( final Scenario scenario );

  /**
   * Returns the scenarios that reside at the root of the project. All other scenarios are derived from any of the root
   * scenarios. The result is never <code>null</code>, but it may be an empty list if there are no scenarios in the
   * project.
   */
  public List<Scenario> getRootScenarios( );

  /**
   * Returns the scenario with the given uri. The scenario may either be a root scenario or a derived scenario. If no
   * scenario with the given uri exists, <code>null</code> will be returned.
   */
  public Scenario getScenario( final String uri );

  /**
   * Removes the scenario and all contained data if the scenario has no derived scenarios.
   */
  public void removeScenario( final Scenario scenario, final IProgressMonitor monitor ) throws CoreException;

  /**
   * Creates a new base scenario with the given name.
   */
  public Scenario createBaseScenario( final String name ) throws CoreException;

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

  /**
   * Registers <code>listener</code> with this scenario manager. If an identical listener is already registered, this
   * method has no effect.
   * 
   * @param listener
   *          the listener to be removed, must not be <code>null</code>
   */
  public void addScenarioManagerListener( final IScenarioManagerListener listener );

  /**
   * Removes <code>listener</code> from this scenario manager. If no identical listener was registered, this method
   * has no effect.
   * 
   * @param listener
   *          the listener to be removed
   */
  public void removeScenarioManagerListener( final IScenarioManagerListener listener );

  /**
   * Deregisters all listeners
   */
  public void dispose( );
}
