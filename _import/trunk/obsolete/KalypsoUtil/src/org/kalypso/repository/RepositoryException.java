package org.kalypso.repository;

/**
 * An Exception that can occur while working with repositories.
 * 
 * @author schlienger
 */
public class RepositoryException extends Exception
{
  public RepositoryException()
  {
    super();
  }

  public RepositoryException( String arg0 )
  {
    super( arg0 );
  }

  public RepositoryException( Throwable arg0 )
  {
    super( arg0 );
  }

  public RepositoryException( String arg0, Throwable arg1 )
  {
    super( arg0, arg1 );
  }
}
