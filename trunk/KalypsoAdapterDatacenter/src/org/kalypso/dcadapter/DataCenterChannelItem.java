package org.kalypso.dcadapter;

import java.util.Iterator;
import java.util.List;

import org.kalypso.repository.IRepository;
import org.kalypso.repository.IRepositoryItem;
import org.kalypso.repository.IRepositoryItemVisitor;
import org.kalypso.repository.RepositoryException;
import org.kalypso.repository.utils.RepositoryVisitors;

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
  @Override
  public String getName( )
  {
    return m_channel.getName();
  }

  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString( )
  {
    return getName();
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getIdentifier()
   */
  @Override
  public String getIdentifier( )
  {
    return m_parent.getIdentifier() + "." + String.valueOf( m_channel.getID() ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getParent()
   */
  @Override
  public IRepositoryItem getParent( )
  {
    return m_parent;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#hasChildren()
   */
  @Override
  public boolean hasChildren( )
  {
    return m_channel.getTimeseries().size() > 0;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getChildren()
   */
  @Override
  public IRepositoryItem[] getChildren( )
  {
    if( m_children == null )
    {
      final List<Timeserie> ts = m_channel.getTimeseries();
      m_children = new IRepositoryItem[ts.size()];

      int i = 0;
      final Iterator<Timeserie> it = ts.iterator();
      while( it.hasNext() )
        m_children[i++] = new DataCenterTimeserieItem( m_rep, this, it.next() );
    }

    return m_children;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getRepository()
   */
  @Override
  public IRepository getRepository( )
  {
    return m_rep;
  }

  @Override
  public Object getAdapter( @SuppressWarnings("rawtypes") final Class anotherClass )
  {
    return null;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#hasAdapter(java.lang.Class)
   */
  @Override
  public boolean hasAdapter( final Class< ? > adapter )
  {
    return false;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#isMultipleSourceItem()
   */
  @Override
  public boolean isMultipleSourceItem( )
  {
    return false;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#accept(org.kalypso.repository.IRepositoryItemVisitor)
   */
  @Override
  public void accept( final IRepositoryItemVisitor visitor ) throws RepositoryException
  {
    RepositoryVisitors.accept( this, visitor );
  }
}
