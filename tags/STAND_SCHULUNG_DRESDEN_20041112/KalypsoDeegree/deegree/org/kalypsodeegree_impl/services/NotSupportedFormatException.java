/*
 * NotSupportedFormatException.java
 * 
 * Created on 9. November 2002, 22:08
 */

package org.deegree_impl.services;

/**
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 */
public class NotSupportedFormatException extends java.lang.Exception
{

  /**
   * Creates a new instance of <code>NotSupportedFormatException</code>
   * without detail message.
   */
  public NotSupportedFormatException()
  {}

  /**
   * Constructs an instance of <code>NotSupportedFormatException</code> with
   * the specified detail message.
   * 
   * @param msg
   *          the detail message.
   */
  public NotSupportedFormatException( String msg )
  {
    super( msg );
  }
}