package org.kalypso.util.repository;

import java.util.Iterator;
import java.util.List;
import java.util.Vector;

/**
 * 
 * @author schlienger
 */
public abstract class AbstractRepository implements IRepository
{
  private final String m_location;

  private final List m_listeners;

  public AbstractRepository( final String location )
  {
    m_location = location;
    m_listeners = new Vector();
  }

  /**
   * @see org.kalypso.util.repository.IRepository#addRepositoryListener(org.kalypso.util.repository.IRepositoryListener)
   */
  public void addRepositoryListener( IRepositoryListener l )
  {
    m_listeners.add( l );
  }

  /**
   * @see org.kalypso.util.repository.IRepository#fireRepositoryStructureChanged()
   */
  public void fireRepositoryStructureChanged()
  {
    for( Iterator iter = m_listeners.iterator(); iter.hasNext(); )
    {
      IRepositoryListener element = (IRepositoryListener)iter.next();

      element.onRepositoryStructureChanged();
    }
  }

  /**
   * @see org.kalypso.util.repository.IRepository#removeRepositoryListener(org.kalypso.util.repository.IRepositoryListener)
   */
  public void removeRepositoryListener( IRepositoryListener l )
  {
    m_listeners.remove( l );
  }

  /**
   * @see org.kalypso.util.repository.IRepository#getLocation()
   */
  public String getLocation()
  {
    return m_location;
  }

  /**
   * @see org.kalypso.util.repository.IRepositoryItem#getName()
   */
  public String getName()
  {
    return getLocation();
  }

  /**
   * @see org.kalypso.util.repository.IRepositoryItem#getParent()
   */
  public IRepositoryItem getParent()
  {
    return null;
  }

  /**
   * @see java.lang.Object#toString()
   */
  public String toString()
  {
    return getName();
  }
}