package org.kalypso.util.loader;

/**
 * Exceptionklasse für Exceptions innerhalb diesesPakets.
 *
 * @author Schlienger
 *
 */
public class LoaderException extends Exception
{
  public LoaderException()
  {
    super();
  }

  public LoaderException( String arg0 )
  {
    super( arg0 );
  }

  public LoaderException( Throwable arg0 )
  {
    super( arg0 );
  }

  public LoaderException( String arg0, Throwable arg1 )
  {
    super( arg0, arg1 );
  }
}
