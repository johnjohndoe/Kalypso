package org.kalypso.users;

/**
 * A Kalypso User
 * 
 * @author schlienger
 */
public class User
{
  private final String[] m_rights;
  private final String m_userName;

  public User( final String userName, final String[] rights )
  {
    m_userName = userName;
    m_rights = rights;
  }

  public String toString( )
  {
    return m_userName;
  }
  
  public boolean hasRight( final String right )
  {
    for( int i = 0; i <m_rights.length; i++ )
    {
      if( right.equals( m_rights[i] ) )
        return true;
    }

    return false;
  }
}
