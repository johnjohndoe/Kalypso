package org.kalypso.ogc.sensor.timeseries.wq.wechmann;

/**
 * Exception while using the Wechmann function.
 * 
 * @author schlienger
 */
public class WechmannException extends Exception
{
  public WechmannException()
  {
    super();
  }

  public WechmannException( String message )
  {
    super( message );
  }

  public WechmannException( Throwable cause )
  {
    super( cause );
  }

  public WechmannException( String message, Throwable cause )
  {
    super( message, cause );
  }
}
