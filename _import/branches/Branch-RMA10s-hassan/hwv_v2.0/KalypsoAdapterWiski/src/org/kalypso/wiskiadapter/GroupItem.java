package org.kalypso.wiskiadapter;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import org.kalypso.repository.IRepository;
import org.kalypso.repository.IRepositoryItem;
import org.kalypso.repository.RepositoryException;
import org.kalypso.wiskiadapter.wiskicall.GetGroupEntryList;
import org.kalypso.wiskiadapter.wiskicall.GetTsInfoList;

/**
 * GroupItem
 * 
 * @author Schlienger
 */
public class GroupItem implements IRepositoryItem
{
  private final String m_id;

  private final String m_name;

  private final SuperGroupItem m_parent;

  private final WiskiRepository m_rep;

  private IRepositoryItem[] m_children;

  public GroupItem( final SuperGroupItem parent, final String id, final String name )
  {
    m_parent = parent;
    m_id = id;
    m_name = name;

    m_rep = (WiskiRepository)m_parent.getRepository();
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getName()
   */
  public String getName()
  {
    return m_name;
  }

  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString()
  {
    return getName();
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getIdentifier()
   */
  public String getIdentifier()
  {
    return m_rep.getIdentifier() + m_id;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getParent()
   */
  public IRepositoryItem getParent()
  {
    return m_parent;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#hasChildren()
   */
  public boolean hasChildren()
  {
    return true;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getChildren()
   */
  @SuppressWarnings("unchecked")
  public IRepositoryItem[] getChildren() throws RepositoryException
  {
    if( m_children == null )
    {
      try
      {
        final GetTsInfoList call = new GetTsInfoList( m_id );
        m_rep.executeWiskiCall( call );

        final GetGroupEntryList call2 = new GetGroupEntryList( m_id );
        m_rep.executeWiskiCall( call2 );

        final List children = new ArrayList( call.getResultList().size() );
        for( final Iterator<HashMap<Object, Object>> it = call.getResultList().iterator(); it.hasNext(); )
        {
          final HashMap<Object, Object> map = it.next();

          final TsInfoItem tsInfoItem = new TsInfoItem( this, map, call2 );
          // Only add active items
          if( tsInfoItem.isActive() )
            children.add( tsInfoItem );
        }

        m_children = (IRepositoryItem[])children.toArray( new IRepositoryItem[children.size()] );
      }
      catch( final Exception e ) // KiWWException and RemoteException
      {
        throw new RepositoryException( e );
      }
    }

    return m_children;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getRepository()
   */
  public IRepository getRepository()
  {
    return m_rep;
  }

  public Object getAdapter( final Class anotherClass )
  {
    // nicht adaptable
    return null;
  }

  /**
   * Find a TsInfoItem (child of this group) using a filter with the given property and value
   * 
   * <p>
   * TODO: why not search insider our children?
   * 
   * @return null if not found
   */
  public TsInfoItem findTsInfo( final String property, final String value ) throws RepositoryException
  {
    try
    {
      final GetTsInfoList call = new GetTsInfoList( m_id, property, value );
      m_rep.executeWiskiCall( call );

      final GetGroupEntryList call2 = new GetGroupEntryList( m_id );
      m_rep.executeWiskiCall( call2 );

      final List list = call.getResultList();
      if( list.size() == 0 )
        return null;

      /* return first active ts-info-item */
      for( int ii = 0; ii < list.size(); ii++ )
      {
        final HashMap map = (HashMap) list.get( ii );
        final TsInfoItem tsInfoItem = new TsInfoItem( this, map, call2 );
        if( tsInfoItem.isActive() )
          return tsInfoItem;
      }

      return null;
    }
    catch( final Exception e ) // KiWWException and RemoteException
    {
      throw new RepositoryException( e );
    }
  }
}
