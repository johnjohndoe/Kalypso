package org.kalypso.repository;

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

  public AbstractRepository()
  {
    this( "" );
  }
  
  public AbstractRepository( final String location )
  {
    m_location = location;
    m_listeners = new Vector();
  }

  /**
   * @see org.kalypso.repository.IRepository#addRepositoryListener(org.kalypso.repository.IRepositoryListener)
   */
  public void addRepositoryListener( IRepositoryListener l )
  {
    m_listeners.add( l );
  }

  /**
   * @see org.kalypso.repository.IRepository#fireRepositoryStructureChanged()
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
   * @see org.kalypso.repository.IRepository#removeRepositoryListener(org.kalypso.repository.IRepositoryListener)
   */
  public void removeRepositoryListener( IRepositoryListener l )
  {
    m_listeners.remove( l );
  }

  /**
   * @see org.kalypso.repository.IRepository#getLocation()
   */
  public String getLocation()
  {
    return m_location;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getName()
   */
  public String getName()
  {
    return getLocation();
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getParent()
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
  
  /**
   * @see org.kalypso.util.adapter.IAdaptable#getAdapter(java.lang.Class)
   */
  public Object getAdapter( Class anotherClass )
  {
    return null;
  }
}