package de.renew.workflow.connector.cases;

/**
 * @author Stefan Kurzbach
 */
public interface ICaseManagerListener<T extends ICase>
{
  public void caseAdded( final T caze );

  public void caseRemoved( final T caze );
}
