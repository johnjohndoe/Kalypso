package org.kalypso.repository;

import java.util.Iterator;
import java.util.List;
import java.util.Properties;
import java.util.Vector;

/**
 * Abstract implementation of <code>IRepository</code> to provide basic
 * functionality.
 * 
 * @author schlienger
 */
public abstract class AbstractRepository implements IRepository
{
  protected String m_location;

  private final List m_listeners;

  private boolean m_readOnly;

  private final Properties m_properties;

  private final IRepositoryFactory m_factory;

  /**
   * Default constructor, location is empty and readonly is true.
   */
  public AbstractRepository( final IRepositoryFactory factory )
  {
    this( factory, "", true );
  }

  /**
   * Constructor
   * 
   * @param factory the <code>IRepositoryFactory</code> that created this repository
   * @param location the location of the repository
   * @param readOnly true if the repository should be in readonly mode
   */
  public AbstractRepository( final IRepositoryFactory factory, final String location, final boolean readOnly )
  {
    m_factory = factory;
    
    m_location = location;
    m_readOnly = readOnly;

    m_listeners = new Vector();
    m_properties = new Properties();
  }

  /**
   * @see org.kalypso.repository.IRepository#getFactory()
   */
  public IRepositoryFactory getFactory()
  {
    return m_factory;
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

  /**
   * @see org.kalypso.repository.IRepository#getProperty(java.lang.String,
   *      java.lang.String)
   */
  public String getProperty( final String name, final String defaultValue )
  {
    return m_properties.getProperty( name, defaultValue );
  }

  /**
   * @see org.kalypso.repository.IRepository#getProperty(java.lang.String)
   */
  public String getProperty( final String name )
  {
    return m_properties.getProperty( name );
  }

  /**
   * @see org.kalypso.repository.IRepository#setProperty(java.lang.String,
   *      java.lang.String)
   */
  public void setProperty( final String name, final String value )
  {
    m_properties.setProperty( name, value );
  }
}