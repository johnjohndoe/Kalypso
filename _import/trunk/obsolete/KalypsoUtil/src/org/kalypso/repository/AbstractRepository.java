package org.kalypso.repository;

import java.util.Iterator;
import java.util.List;
import java.util.Vector;

/**
 * Abstract implementation of <code>IRepository</code> to provide basic functionality.
 * 
 * @author schlienger
 */
public abstract class AbstractRepository implements IRepository
{
  protected String m_location;

  private final List m_listeners;

  private boolean m_readOnly;

  /**
   * Default constructor, location is empty and readonly is true.
   */
  public AbstractRepository()
  {
    this( "", true );
  }
  
  public AbstractRepository( final String location, final boolean readOnly )
  {
    m_location = location;
    m_readOnly = readOnly;
    
    m_listeners = new Vector();
  }

  /**
   * @see org.kalypso.repository.IRepository#isReadOnly()
   */
  public boolean isReadOnly()
  {
    return m_readOnly;
  }
  
  public void setReadOnly( final boolean ro )
  {
    m_readOnly = ro;
  }
  
  /**
   * @see org.kalypso.repository.IRepository#addRepositoryListener(org.kalypso.repository.IRepositoryListener)
   */
  public void addRepositoryListener( final IRepositoryListener l )
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
  public void removeRepositoryListener( final IRepositoryListener l )
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
   * This default implementation always returns null.
   * 
   * @see org.kalypso.util.adapter.IAdaptable#getAdapter(java.lang.Class)
   */
  public Object getAdapter( final Class anotherClass )
  {
    return null;
  }
  
  /**
   * @see org.kalypso.repository.IRepositoryItem#getRepository()
   */
  public IRepository getRepository()
  {
    return this;
  }
}