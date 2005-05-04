package org.kalypso.ui.application.login;

import org.kalypso.users.User;

public interface ILoginValidator
{
  public boolean userNameChangeable( );

  public boolean passwordEnabled( );

  public String getDefaultUserName( );

  public String getMessage( );

  public User validate( final String username, final String password )
      throws Exception;
}