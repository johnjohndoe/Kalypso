package org.kalypso.util.repository;

/**
 * 
 * @author schlienger
 */
public abstract class AbstractRepository implements IRepository
{
  private final String m_identifier;

  private final String m_location;

  public AbstractRepository( final String identifier, final String location )
  {
    m_identifier = identifier;
    m_location = location;
  }

  /**
   * @see org.kalypso.util.repository.IRepository#getIdentifier()
   */
  public String getIdentifier()
  {
    return m_identifier;
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