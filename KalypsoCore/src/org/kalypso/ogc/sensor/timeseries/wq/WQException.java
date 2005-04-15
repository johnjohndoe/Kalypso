package org.kalypso.ogc.sensor.timeseries.wq;

/**
 * WQTableException
 * 
 * @author schlienger
 */
public class WQException extends Exception
{
  public WQException( )
  {
    super();
  }

  public WQException( String message )
  {
    super( message );
  }

  public WQException( Throwable cause )
  {
    super( cause );
  }

  public WQException( String message, Throwable cause )
  {
    super( message, cause );
  }
}
