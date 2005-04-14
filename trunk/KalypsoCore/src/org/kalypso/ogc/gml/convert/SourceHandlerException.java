package org.kalypso.ogc.gml.convert;

/**
 * @author belger
 */
public class SourceHandlerException extends Exception
{
  public SourceHandlerException()
  {
    super();
  }

  public SourceHandlerException( String message )
  {
    super( message );
  }

  public SourceHandlerException( Throwable cause )
  {
    super( cause );
  }

  public SourceHandlerException( String message, Throwable cause )
  {
    super( message, cause );
  }

}
