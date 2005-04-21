package org.kalypso.wiskiadapter;

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import org.kalypso.repository.IRepository;
import org.kalypso.repository.IRepositoryItem;
import org.kalypso.repository.RepositoryException;

import de.kisters.tsmsystem.common.data.SimpleRequestFilterTerm;
import de.kisters.tsmsystem.common.data.SimpleRequestSortTerm;
import de.kisters.wiski.webdataprovider.common.net.KiWWDataProviderInterface;

/**
 * SuperGroupItem
 * 
 * @author schlienger
 */
public class SuperGroupItem implements IRepositoryItem
{
  private final String m_name;

  private final WiskiRepository m_rep;

  public SuperGroupItem( final WiskiRepository repository, final String name )
  {
    m_rep = repository;
    m_name = name;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getName()
   */
  public String getName( )
  {
    return m_name;
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
    return m_rep.getIdentifier() + m_name;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getParent()
   */
  public IRepositoryItem getParent( ) throws RepositoryException
  {
    return m_rep;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#hasChildren()
   */
  public boolean hasChildren( ) throws RepositoryException
  {
    return true;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getChildren()
   */
  public IRepositoryItem[] getChildren( ) throws RepositoryException
  {
    final SimpleRequestFilterTerm filtergroup = new SimpleRequestFilterTerm();
    filtergroup.addColumnReference( "supergroup_name" );
    filtergroup.addOperator( "like" );
    filtergroup.addValue( m_name );

    final SimpleRequestSortTerm sort = new SimpleRequestSortTerm();
    sort.addColumnAscent( "group_name" );

    try
    {
      final HashMap grouplist = m_rep.getWiski().getGroupList(
          m_rep.getUserData(), GroupItem.COLUMNS,
          KiWWDataProviderInterface.TIMESERIES_GROUP, sort, filtergroup, 15, 0,
          false, null );

      final List resultList = (List) grouplist
          .get( KiWWDataProviderInterface.KEY_RESULT_LIST );

      final GroupItem[] groups = new GroupItem[resultList.size()];
      int i = 0;
      for( final Iterator it = resultList.iterator(); it.hasNext(); )
      {
        final HashMap map = (HashMap) it.next();
        groups[i++] = new GroupItem( this, (String) map.get( "group_id" ),
            (String) map.get( "group_name" ) );
      }

      return groups;
    }
    catch( Exception e ) // RemoteException or KiWWException
    {
      e.printStackTrace();
      throw new RepositoryException( e );
    }
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
  public Object getAdapter( final Class anotherClass )
  {
    // nicht adaptable
    return null;
  }
}
