package org.kalypso.dcadapter;

import java.util.Iterator;
import java.util.List;

import org.kalypso.repository.IRepository;
import org.kalypso.repository.IRepositoryItem;
import org.kalypso.repository.RepositoryException;

import com.bce.datacenter.db.timeseries.Channel;
import com.bce.datacenter.db.timeseries.Timeserie;

/**
 * DataCenterChannelItem
 * 
 * @author marc
 */
public class DataCenterChannelItem implements IRepositoryItem
{
  private final DataCenterRepository m_rep;
  private final Channel m_channel;
  private final DataCenterLevelItem m_parent;
  private IRepositoryItem[] m_children;

  public DataCenterChannelItem( final DataCenterRepository rep, final DataCenterLevelItem parent, final Channel channel )
  {
    m_rep = rep;
    m_parent = parent;
    m_channel = channel;
  }

  public Channel getChannel( )
  {
    return m_channel;
  }
  
  /**
   * @see org.kalypso.repository.IRepositoryItem#getName()
   */
  public String getName( )
  {
    return m_channel.getName();
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
    return String.valueOf( m_channel.getID() );
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
    return m_channel.getTimeseries().size() > 0;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getChildren()
   */
  public IRepositoryItem[] getChildren( ) throws RepositoryException
  {
    if( m_children == null )
    {
      final List ts = m_channel.getTimeseries();
      m_children = new IRepositoryItem[ts.size()];
      
      int i = 0;
      final Iterator it = ts.iterator();
      while( it.hasNext() )
        m_children[i++] = new DataCenterTimeserieItem( m_rep, this, (Timeserie) it.next() );
    }
    
    return m_children;
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
    return null;
  }
}
