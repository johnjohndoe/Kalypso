package org.kalypso.wiskiadapter;

import java.util.HashMap;
import java.util.Iterator;

import org.kalypso.repository.IRepository;
import org.kalypso.repository.IRepositoryItem;
import org.kalypso.repository.RepositoryException;
import org.kalypso.wiskiadapter.wiskicall.GetGroupList;

/**
 * SuperGroupItem
 * 
 * @author schlienger
 */
public class SuperGroupItem implements IRepositoryItem
{
  private final String m_name;

  private final WiskiRepository m_rep;

  private IRepositoryItem[] m_children;

  public SuperGroupItem( final WiskiRepository repository, final String name )
  {
    m_rep = repository;
    m_name = name;
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
  public String toString()
  {
    return getName();
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getIdentifier()
   */
  public String getIdentifier()
  {
    return m_rep.getIdentifier() + m_name;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getParent()
   */
  public IRepositoryItem getParent() throws RepositoryException
  {
    return m_rep;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#hasChildren()
   */
  public boolean hasChildren() throws RepositoryException
  {
    return true;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getChildren()
   */
  public IRepositoryItem[] getChildren() throws RepositoryException
  {
    if( m_children == null )
    {
      try
      {
        final GetGroupList call = new GetGroupList( m_name );
        m_rep.executeWiskiCall( call );

        m_children = new GroupItem[call.getResultList().size()];
        int i = 0;
        for( final Iterator it = call.getResultList().iterator(); it.hasNext(); )
        {
          final HashMap map = (HashMap)it.next();
          m_children[i++] = new GroupItem( this, (String)map.get( "group_id" ),
              (String)map.get( "group_name" ) );
        }
      }
      catch( final Exception e ) // RemoteException or KiWWException
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

  /**
   * @see org.kalypso.util.adapter.IAdaptable#getAdapter(java.lang.Class)
   */
  public Object getAdapter( final Class anotherClass )
  {
    // nicht adaptable
    return null;
  }
}
