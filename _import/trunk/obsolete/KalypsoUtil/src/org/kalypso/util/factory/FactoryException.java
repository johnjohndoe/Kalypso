package org.kalypso.util.factory;

/**
 * Exceptionklasse für Exceptions innerhalb des Factory Packages.
 *
 * @author schlienger
 */
public class FactoryException extends Exception
{
  public FactoryException()
  {
    super();
  }

  public FactoryException( String arg0 )
  {
    super( arg0 );
  }

  public FactoryException( Throwable arg0 )
  {
    super( arg0 );
  }

  public FactoryException( String arg0, Throwable arg1 )
  {
    super( arg0, arg1 );
  }
}
