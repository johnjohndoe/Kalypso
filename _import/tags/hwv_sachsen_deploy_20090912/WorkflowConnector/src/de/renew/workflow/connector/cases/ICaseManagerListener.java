package de.renew.workflow.connector.cases;

import de.renew.workflow.cases.Case;

/**
 * @author Stefan Kurzbach
 */
public interface ICaseManagerListener<T extends Case>
{
  public void caseAdded( final T caze );

  public void caseRemoved( final T caze );
}
