package org.kalypso.jts;

public class CoordOrientationException extends Exception
{
  /**
   * @param message
   */
  public CoordOrientationException( final String message )
  {
    super( message );
  }

  /**
   * @param message
   * @param cause
   */
  public CoordOrientationException( final String message, final Throwable cause )
  {
    super( message, cause );
  }
}
