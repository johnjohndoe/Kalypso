/*
 * CatalogClientException.java
 * 
 * Created on 6. November 2003, 22:15
 */

package org.deegree_impl.clients.wcasclient;

import org.deegree_impl.tools.StringExtend;

/**
 * 
 * @author Administrator
 */
public class MissingISO19119EntryException extends CatalogClientException
{

  private String st = "";

  /**
   * Creates a new instance of <code>CatalogClientException</code> without
   * detail message.
   */
  public MissingISO19119EntryException()
  {}

  /**
   * Constructs an instance of <code>CatalogClientException</code> with the
   * specified detail message.
   * 
   * @param msg
   *          the detail message.
   */
  public MissingISO19119EntryException( String msg )
  {
    super( msg );
  }

  /**
   * Constructs an instance of <code>CatalogClientException</code> with the
   * specified detail message.
   * 
   * @param msg
   *          the detail message.
   */
  public MissingISO19119EntryException( String msg, Exception e )
  {
    this( msg );
    st = StringExtend.stackTraceToString( e.getStackTrace() );
  }

  public String toString()
  {
    return super.toString() + "\n" + st;
  }

}