package org.kalypso.ui.application.login;

import org.kalypso.users.User;
import org.kalypso.users.UserServiceConstants;

public class AppLoginValidator implements ILoginValidator
{
  public User validate( final String username, final String password )
  {
    if( "hochwasser".equals( password ) )
      return new User( "Administrator",
          new String[] { UserServiceConstants.RIGHT_ADMIN } );

    return null;
  }

  public boolean userNameChangeable( )
  {
    return false;
  }

  public boolean passwordEnabled( )
  {
    return true;
  }

  public String getDefaultUserName( )
  {
    return "Administrator";
  }

  public String getMessage( )
  {
    return "Es konnten keine Benutzerrechte vom Server ermittelt werden.\n"
        + "Geben Sie das Administrator-Passwort ein, um im Administrator-Modus zu starten.";
  }
}