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
  
  private final IScenario[] m_scenarios;

  private final String m_scenarioID;

  public KalypsoUser( final String userName, final String[] rights, final String scenarioID, final IScenario[] scenarios )
  {
    m_userName = userName;
    m_rights = rights;
    m_scenarioID = scenarioID;
    m_scenarios = scenarios;
  }
  
  public String getUserName()
  {
    return m_userName;
  }

  @Override
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
   * @see org.kalypso.auth.user.IKalypsoUser#getScenario()
   */
  public String getScenario()
  {
    return m_scenarioID;
  }

  /**
   * @see org.kalypso.auth.scenario.IScenarioProvider#getAvailableScenarios()
   */
  public IScenario[] getAvailableScenarios()
  {
    return m_scenarios;
  }
}
