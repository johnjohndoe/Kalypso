package org.kalypso.util.xml.xlink;

/**
 * An Exception that can occur while using IXLinks and related objects.
 * 
 * @author schlienger
 */
public class XLinkException extends Exception
{
  public XLinkException()
  {
    super();
  }

  public XLinkException( String message )
  {
    super( message );
  }

  public XLinkException( Throwable cause )
  {
    super( cause );
  }

  public XLinkException( String message, Throwable cause )
  {
    super( message, cause );
  }
}
