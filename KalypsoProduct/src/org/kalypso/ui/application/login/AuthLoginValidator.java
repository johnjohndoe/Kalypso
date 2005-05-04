package org.kalypso.ui.application.login;

import java.rmi.RemoteException;

import org.kalypso.services.proxy.IUserService;
import org.kalypso.users.User;

public class AuthLoginValidator implements ILoginValidator
{
  private final IUserService m_srv;
  

  public AuthLoginValidator( final IUserService srv )
  {
    m_srv = srv;
  }

  public User validate( final String username, final String password )
      throws Exception
  {
    try
    {
      final String[] rights = m_srv.getRights2( username, password );

      if( rights != null )
        return new User( username, rights );
    }
    catch( final RemoteException e )
    {
      throw e;
    }

    return null;
  }

  public boolean userNameChangeable( )
  {
    return true;
  }

  public boolean passwordEnabled( )
  {
    return true;
  }

  public String getDefaultUserName( )
  {
    return System.getProperty( "user.name" );
  }

  public String getMessage( )
  {
    return "Melden Sie sich bitte an.";
  }
}