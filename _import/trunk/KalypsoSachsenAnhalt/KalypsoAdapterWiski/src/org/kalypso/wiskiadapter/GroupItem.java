package org.kalypso.wiskiadapter;

import java.util.HashMap;
import java.util.Iterator;

import org.kalypso.repository.IRepository;
import org.kalypso.repository.IRepositoryItem;
import org.kalypso.repository.RepositoryException;
import org.kalypso.wiskiadapter.wiskicall.GetTsInfoList;

/**
 * GroupItem
 * 
 * @author schlienger
 */
public class GroupItem implements IRepositoryItem
{
  private final String m_id;

  private final String m_name;

  private final SuperGroupItem m_parent;

  private final WiskiRepository m_rep;

  public GroupItem( final SuperGroupItem parent, final String id,
      final String name )
  {
    m_parent = parent;
    m_id = id;
    m_name = name;

    m_rep = (WiskiRepository) m_parent.getRepository();
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
    return m_rep.getIdentifier() + m_id;
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
    try
    {
      final GetTsInfoList call = new GetTsInfoList( m_id );
      m_rep.executeWiskiCall( call );
      final TsInfoItem[] tsitems = new TsInfoItem[call.getResultList().size()];

      int i = 0;
      for( final Iterator it = call.getResultList().iterator(); it.hasNext(); )
      {
        final HashMap map = (HashMap) it.next();

        tsitems[i++] = new TsInfoItem( this, map );
      }

      return tsitems;
    }
    catch( final Exception e ) // KiWWException and RemoteException
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
  public Object getAdapter( Class anotherClass )
  {
    // nicht adaptable
    return null;
  }
}
