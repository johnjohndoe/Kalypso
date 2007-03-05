/**
 * 
 */
package org.kalypso.afgui.db;

import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.kalypso.scenarios.Scenario;

/**
 * Represents the workflow data base
 * 
 * @author Patrice Congo, Stefan Kurzbach
 */
public interface IWorkflowDB
{
  public List<Scenario> getRootScenarios();
  
  public Scenario getScenario( final String id );

  public Scenario deriveScenario( final String id, final Scenario parentScenario ) throws CoreException;

  public void persist( ) throws CoreException;

  public void dispose( );

  public void addWorkflowDBChangeListener( final IWorkflowDBChangeListerner listener );

  public void removeWorkflowDBChangeListener( final IWorkflowDBChangeListerner listener );
}
