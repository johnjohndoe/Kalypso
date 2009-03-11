package org.kalypso.ogc.gml.convert;

/**
 * @author belger
 */
public class GmlConvertException extends Exception
{
  public GmlConvertException()
  {
    super();
  }

  public GmlConvertException( String message )
  {
    super( message );
  }

  public GmlConvertException( Throwable cause )
  {
    super( cause );
  }

  public GmlConvertException( String message, Throwable cause )
  {
    super( message, cause );
  }

}
