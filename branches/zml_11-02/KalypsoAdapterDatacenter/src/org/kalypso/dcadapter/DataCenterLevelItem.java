package org.kalypso.dcadapter;

import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Iterator;

import org.kalypso.repository.IRepository;
import org.kalypso.repository.IRepositoryItem;
import org.kalypso.repository.IRepositoryItemVisitor;
import org.kalypso.repository.RepositoryException;
import org.kalypso.repository.utils.RepositoryVisitors;

import com.bce.datacenter.db.common.Level;
import com.bce.datacenter.db.timeseries.Channel;

/**
 * DataCenterItem
 * 
 * @author marc
 */
public class DataCenterLevelItem implements IRepositoryItem
{
  private final Level m_level;

  private IRepositoryItem[] m_children = null;

  private final DataCenterRepository m_rep;

  private final DataCenterLevelItem m_parent;

  public DataCenterLevelItem( final DataCenterRepository rep, final DataCenterLevelItem parent, final Level level )
  {
    m_rep = rep;
    m_parent = parent;
    m_level = level;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getName()
   */
  @Override
  public String getName( )
  {
    return m_level.getName();
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getIdentifier()
   */
  @Override
  public String getIdentifier( )
  {
    if( m_parent != null )
      return m_parent.getIdentifier() + "." + String.valueOf( m_level.getID() ); //$NON-NLS-1$

    return m_rep.getIdentifier() + String.valueOf( m_level.getID() );
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
    return m_level.getChildCount() > 0 || m_level.getObjects().size() > 0;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getChildren()
   */
  @Override
  public IRepositoryItem[] getChildren( )
  {
    if( m_children == null )
    {
      final ArrayList<IRepositoryItem> list = new ArrayList<IRepositoryItem>();
      final Enumeration<Level> cs = m_level.children();
      while( cs.hasMoreElements() )
        list.add( new DataCenterLevelItem( m_rep, this, cs.nextElement() ) );

      final Iterator<Channel> it = m_level.getObjects().iterator();
      while( it.hasNext() )
        list.add( new DataCenterChannelItem( m_rep, this, it.next() ) );

      m_children = list.toArray( new IRepositoryItem[list.size()] );
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
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString( )
  {
    return getName();
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