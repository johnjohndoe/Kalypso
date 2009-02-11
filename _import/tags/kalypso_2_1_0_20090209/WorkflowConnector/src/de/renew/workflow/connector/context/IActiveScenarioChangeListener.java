package de.renew.workflow.connector.context;

import de.renew.workflow.cases.Case;
import de.renew.workflow.connector.cases.CaseHandlingProjectNature;

/**
 * Interface to implement in order to be notified if the active scenario has changed.
 * 
 * @author Patrice Congo, Stefan Kurzbach
 */
public interface IActiveScenarioChangeListener<T extends Case>
{
  public void activeScenarioChanged( final CaseHandlingProjectNature newProject, final T caze );
}