package org.kalypso.dcadapter;

import org.kalypso.repository.IRepository;
import org.kalypso.repository.IRepositoryItem;
import org.kalypso.repository.RepositoryException;

import com.bce.datacenter.db.timeseries.Timeserie;

/**
 * DataCenterTimeserieItem
 * 
 * @author marc
 */
public class DataCenterTimeserieItem implements IRepositoryItem
{
  private final DataCenterRepository m_rep;
  private final DataCenterChannelItem m_parent;
  private final Timeserie m_ts;

  public DataCenterTimeserieItem( final DataCenterRepository rep, final DataCenterChannelItem parent, final Timeserie ts )
  {
    m_rep = rep;
    m_parent = parent;
    m_ts = ts;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getName()
   */
  public String getName( )
  {
    return m_ts.getName();
  }

  /**
   * @see java.lang.Object#toString()
   */
  public String toString( )
  {
    return getName();
  }
  
  /**
   * @see org.kalypso.repository.IRepositoryItem#getIdentifier()
   */
  public String getIdentifier( )
  {
    return String.valueOf( m_ts.getID() );
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getParent()
   */
  public IRepositoryItem getParent( ) throws RepositoryException
  {
    return m_parent;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#hasChildren()
   */
  public boolean hasChildren( ) throws RepositoryException
  {
    return false;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getChildren()
   */
  public IRepositoryItem[] getChildren( ) throws RepositoryException
  {
    return null;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getRepository()
   */
  public IRepository getRepository( )
  {
    return m_rep;
  }

  /**
   * @see org.kalypso.util.adapter.IAdaptable#getAdapter(java.lang.Class)
   */
  public Object getAdapter( Class anotherClass )
  {
    // TODO observation
    return null;
  }
}
