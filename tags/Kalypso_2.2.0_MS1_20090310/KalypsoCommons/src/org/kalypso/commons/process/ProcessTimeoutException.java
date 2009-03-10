package org.kalypso.commons.process;

/**
 * This exception happens, if the timeout for a process is reached.<br>
 * TODO: auto generate message from timeout string + process name
 * 
 * @author Gernot Belger
 */
public class ProcessTimeoutException extends Exception
{
  public ProcessTimeoutException( )
  {
    super();
  }

  public ProcessTimeoutException( final String message )
  {
    super( message );
  }

  public ProcessTimeoutException( final Throwable cause )
  {
    super( cause );
  }

  public ProcessTimeoutException( final String message, final Throwable cause )
  {
    super( message, cause );
  }
}