package org.kalypso.psiadapter.repository;

import java.util.List;
import java.util.Vector;

import org.kalypso.repository.IRepositoryItem;

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

  public PSICompactItem( final PSICompactItem parent, final String name )
  {
    m_parent = parent;
    m_name = name;
    
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
}
