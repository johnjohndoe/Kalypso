package org.kalypso.ogc.sensor;

/**
 * Eine Exception Klasse für die Exceptions bezüglich dieses sensor Package.
 * 
 * @author schlienger
 */
public class SensorException extends Exception
{
  public SensorException()
  {
    super();
  }

  public SensorException( String message )
  {
    super( message );
  }

  public SensorException( Throwable cause )
  {
    super( cause );
  }

  public SensorException( String message, Throwable cause )
  {
    super( message, cause );
  }
}
