package org.kalypso.java.lang;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

/**
 * MultiException can contain a list of exception. It is used as a container for exceptions that might occur during a
 * loop processing, but that should not break the loop. Better, the exceptions that occur are caught and added to this
 * container. Once the loop processing is finished, the method isEmpty() can be called to check if it is necessary to
 * throw the MultiException or not.
 * 
 * @author schlienger
 */
public class MultiException extends Exception
{
  private List m_exceptions = null;

  public MultiException()
  {
    super();
  }

  /**
   * @see java.lang.Object#finalize()
   */
  protected void finalize() throws Throwable
  {
    if( !isEmpty() )
      m_exceptions.clear();

    super.finalize();
  }

  public void addException( Exception e )
  {
    if( m_exceptions == null )
      m_exceptions = new LinkedList();

    m_exceptions.add( e );
  }

  /**
   * @see java.lang.Throwable#getMessage()
   */
  public String getMessage()
  {
    if( isEmpty() )
      return "";

    final StringBuffer bf = new StringBuffer();
    for( final Iterator it = m_exceptions.iterator(); it.hasNext(); )
      bf.append( ( (Exception)it.next() ).getMessage() ).append( '\n' );

    return bf.toString();
  }

  /**
   * @return true if this MultiException container does not contain any exceptions
   */
  public boolean isEmpty()
  {
    return m_exceptions == null || m_exceptions.size() == 0;
  }
}
