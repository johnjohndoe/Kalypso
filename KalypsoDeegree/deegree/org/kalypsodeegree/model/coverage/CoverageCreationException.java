/*
 * CoverageCreationException.java
 * 
 * Created on 13. Februar 2003, 16:54
 */

package org.deegree.model.coverage;

/**
 * 
 * @author Administrator
 */
public class CoverageCreationException extends java.lang.Exception
{

  /**
   * Creates a new instance of <code>CoverageCreationException</code> without
   * detail message.
   */
  public CoverageCreationException()
  {}

  /**
   * Constructs an instance of <code>CoverageCreationException</code> with the
   * specified detail message.
   * 
   * @param msg
   *          the detail message.
   */
  public CoverageCreationException( String msg )
  {
    super( msg );
  }
}