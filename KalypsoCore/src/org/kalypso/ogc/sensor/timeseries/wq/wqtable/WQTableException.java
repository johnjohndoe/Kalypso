package org.kalypso.ogc.sensor.timeseries.wq.wqtable;

/**
 * WQTableException
 * 
 * @author schlienger
 */
public class WQTableException extends Exception
{
  public WQTableException( )
  {
    super();
  }

  public WQTableException( String message )
  {
    super( message );
  }

  public WQTableException( Throwable cause )
  {
    super( cause );
  }

  public WQTableException( String message, Throwable cause )
  {
    super( message, cause );
  }
}
