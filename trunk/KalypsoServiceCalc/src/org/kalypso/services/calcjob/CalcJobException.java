package org.kalypso.services.calcjob;

/**
 * @author gernot
 */
public class CalcJobException extends Exception
{
  public CalcJobException( String message )
  {
    super( message );
  }

  public CalcJobException( String message, Throwable cause )
  {
    super( message, cause );
  }

  public CalcJobException( Throwable cause )
  {
    super( cause );
  }
}
