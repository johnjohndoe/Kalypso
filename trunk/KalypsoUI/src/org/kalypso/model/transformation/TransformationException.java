package org.kalypso.model.transformation;

/**
 * @author belger
 */
public class TransformationException extends Exception
{
  public TransformationException()
  {
    super();
  }

  public TransformationException( String arg0 )
  {
    super( arg0 );
  }

  public TransformationException( Throwable arg0 )
  {
    super( arg0 );
  }

  public TransformationException( String arg0, Throwable arg1 )
  {
    super( arg0, arg1 );
  }
}
