package de.renew.workflow.connector.context;

import de.renew.workflow.connector.cases.CaseHandlingProjectNature;
import de.renew.workflow.connector.cases.ICase;

/**
 * Interface to implement in order to be notified if the active scenario has changed.
 * 
 * @author Patrice Congo, Stefan Kurzbach
 */
public interface IActiveScenarioChangeListener<T extends ICase>
{
  public void activeScenarioChanged( final CaseHandlingProjectNature newProject, final T caze );
}