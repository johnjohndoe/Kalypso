package org.kalypso.java.lang.reflect;


/**
 * An Exception that can occur using the ClassUtilities
 * 
 * @author schlienger
 */
public class ClassUtilityException extends Exception
{
  public ClassUtilityException()
  {
    super();
  }

  public ClassUtilityException( String message )
  {
    super( message );
  }

  public ClassUtilityException( String message, Throwable cause )
  {
    super( message, cause );
  }

  public ClassUtilityException( Throwable cause )
  {
    super( cause );
  }
}