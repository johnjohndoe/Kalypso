package org.kalypso.dcadapter;

import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Iterator;

import org.kalypso.repository.IRepository;
import org.kalypso.repository.IRepositoryItem;
import org.kalypso.repository.RepositoryException;

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
  public String getName( )
  {
    return m_level.getName();
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getIdentifier()
   */
  public String getIdentifier( )
  {
    return String.valueOf( m_level.getID() );
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
    return m_level.getChildCount() > 0 || m_level.getObjects().size() > 0;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getChildren()
   */
  public IRepositoryItem[] getChildren( ) throws RepositoryException
  {
    if( m_children == null )
    {
      final ArrayList list = new ArrayList();
      final Enumeration cs = m_level.children();
      while( cs.hasMoreElements() )
        list.add( new DataCenterLevelItem( m_rep, this, (Level) cs.nextElement() ) );
      
      final Iterator it = m_level.getObjects().iterator();
      while( it.hasNext() )
        list.add( new DataCenterChannelItem( m_rep, this, (Channel)it.next()) );

      m_children = (DataCenterLevelItem[]) list
          .toArray( new DataCenterLevelItem[list.size()] );
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