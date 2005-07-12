package org.kalypso.auth.user;

import org.kalypso.auth.scenario.IScenario;

/**
 * A Kalypso User, default implementation of IKalypsoUser.
 * 
 * @author schlienger
 */
public class KalypsoUser implements IKalypsoUser
{
  private final String[] m_rights;

  private final String m_userName;

  private final IScenario m_scenario;

  public KalypsoUser( final String userName, final String[] rights, final IScenario scenario )
  {
    m_userName = userName;
    m_rights = rights;
    m_scenario = scenario;
  }

  public String toString()
  {
    return m_userName;
  }

  public boolean hasRight( final String right )
  {
    for( int i = 0; i < m_rights.length; i++ )
    {
      if( right.equals( m_rights[i] ) )
        return true;
    }

    return false;
  }

  /**
   * @see org.kalypso.auth.scenario.IScenarioProvider#getScenario()
   */
  public IScenario getScenario()
  {
    return m_scenario;
  }
}
