package org.kalypso.ogc.gml.serialize;

/**
 * @author gernot
 */
public class GmlSerializeException extends Exception
{
  public GmlSerializeException( final String message )
  {
    super( message );
  }

  public GmlSerializeException( final String message, final Throwable cause )
  {
    super( message, cause );
  }

  public GmlSerializeException( final Throwable cause )
  {
    super( cause );
  }
}