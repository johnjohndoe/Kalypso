package org.kalypso.psiadapter.repository;

import java.util.List;
import java.util.Vector;

import org.kalypso.repository.IRepository;
import org.kalypso.repository.IRepositoryItem;
import org.kalypso.repository.RepositoryException;

import de.psi.go.lhwz.PSICompact;
import de.psi.go.lhwz.PSICompact.ObjectInfo;

/**
 * A Repository Item from the PSICompact structure
 * 
 * @author schlienger
 */
public class PSICompactItem implements IRepositoryItem
{
  private final PSICompactItem m_parent;
  private final String m_name;
  private final List m_children;
  protected final ObjectInfo m_objectInfo;

  public PSICompactItem( final PSICompactItem parent, final String name, final PSICompact.ObjectInfo info )
  {
    m_parent = parent;
    m_name = name;
    m_objectInfo = info;
    
    m_children = new Vector();
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

  public void addChild( PSICompactItem item )
  {
    m_children.add( item );
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
    return getChildren().length != 0;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getChildren()
   */
  public IRepositoryItem[] getChildren()
  {
    return (IRepositoryItem[])m_children.toArray( new IRepositoryItem[ m_children.size()] );
  }

  /**
   * @see org.kalypso.util.adapter.IAdaptable#getAdapter(java.lang.Class)
   */
  public Object getAdapter( Class anotherClass )
  {
    return null;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getRepository()
   */
  public IRepository getRepository()
  {
    try
    {
      return PSICompactFactory.getRepository(  );
    }
    catch( RepositoryException e )
    {
      e.printStackTrace();
      
      throw new IllegalStateException( "Invalid repository. See previous stack trace for the RepositoryException" );
    }
  }

  /**
   * Returns <pre>psicompact://psi...id</pre> with psi...id being the id that is delivered 
   * from the PSICompact interface.
   * 
   * @see org.kalypso.repository.IRepositoryItem#getIdentifier()
   */
  public String getIdentifier()
  {
    return getRepository().getIdentifier() + m_objectInfo.getId();
  }
}
