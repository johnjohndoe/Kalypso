package org.kalypso.util.parser;

/**
 * Eine Exception die während die Benutzung von Parsers auftreten kann.
 * 
 * @author schlienger
 */
public class ParserException extends Exception
{
  public ParserException()
  {
    super();
  }

  public ParserException( String message )
  {
    super( message );
  }

  public ParserException( Throwable cause )
  {
    super( cause );
  }

  public ParserException( String message, Throwable cause )
  {
    super( message, cause );
  }
}
