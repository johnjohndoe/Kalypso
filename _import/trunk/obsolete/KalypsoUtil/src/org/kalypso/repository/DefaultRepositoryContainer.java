package org.kalypso.repository;

import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Vector;


/**
 * Beinhaltet eine Liste von Repositories und verwaltet Listeners.
 * 
 * @author schlienger
 */
public class DefaultRepositoryContainer implements IRepositoryContainer
{
  private final List m_reps = new Vector();

  private final List m_listeners = new Vector();

  public DefaultRepositoryContainer()
  {
    this( new IRepository[0] );
  }

  public DefaultRepositoryContainer( IRepository[] repositories )
  {
    m_reps.addAll( Arrays.asList( repositories ) );
  }

  public void addRepository( IRepository rep )
  {
    m_reps.add( rep );

    fireRepositoryChanged();
  }

  private void fireRepositoryChanged()
  {
    for( Iterator iter = m_listeners.iterator(); iter.hasNext(); )
    {
      IRepositoryContainerListener element = (IRepositoryContainerListener)iter.next();
      
      element.onRepositoryContainerChanged();
    }
  }

  public void removeRepository( IRepository rep )
  {
    m_reps.remove( rep );
    
    fireRepositoryChanged();
  }

  public int getRepositoriesCount()
  {
    return m_reps.size();
  }

  /**
   * @see org.kalypso.repository.IRepositoryContainer#addRepositoryContainerListener(org.kalypso.repository.IRepositoryContainerListener)
   */
  public void addRepositoryContainerListener( IRepositoryContainerListener l )
  {
    m_listeners.add( l );
  }

  /**
   * @see org.kalypso.repository.IRepositoryContainer#removeRepositoryContainerListener(org.kalypso.repository.IRepositoryContainerListener)
   */
  public void removeRepositoryContainerListener( IRepositoryContainerListener l )
  {
    m_listeners.remove( l );
  }

  /**
   * @see org.kalypso.repository.IRepositoryContainer#getRepositories()
   */
  public List getRepositories()
  {
    return m_reps;
  }
}