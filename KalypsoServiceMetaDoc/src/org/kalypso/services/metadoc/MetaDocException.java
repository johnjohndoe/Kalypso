package org.kalypso.services.metadoc;

/**
 * MetaDocException
 * 
 * @author schlienger
 */
public class MetaDocException extends Exception
{
  public MetaDocException( )
  {
    super();
  }

  public MetaDocException( String message )
  {
    super( message );
  }

  public MetaDocException( Throwable cause )
  {
    super( cause );
  }

  public MetaDocException( String message, Throwable cause )
  {
    super( message, cause );
  }
}
