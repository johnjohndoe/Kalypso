package org.kalypso.users;

/**
 * UserRightsException for exceptions occuring within this package.
 * 
 * @author schlienger
 */
public class UserRightsException extends Exception
{
  public UserRightsException( )
  {
    super();
  }

  public UserRightsException( String message )
  {
    super( message );
  }

  public UserRightsException( String message, Throwable cause )
  {
    super( message, cause );
  }

  public UserRightsException( Throwable cause )
  {
    super( cause );
  }
}
