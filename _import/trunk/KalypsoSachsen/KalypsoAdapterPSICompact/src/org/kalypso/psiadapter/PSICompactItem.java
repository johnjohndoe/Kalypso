package org.kalypso.psiadapter;

import java.util.List;
import java.util.Vector;

import org.kalypso.ogc.sensor.ITarget;
import org.kalypso.util.repository.IRepositoryItem;

import de.psi.go.lhwz.PSICompact.ObjectInfo;

/**
 * A Repository Item from the PSICompact structure
 * 
 * @author schlienger
 */
public class PSICompactItem implements IRepositoryItem, ITarget
{
  private final PSICompactItem m_parent;
  private final String m_name;
  private final List m_children;
  private final ObjectInfo m_objectInfo;

  public PSICompactItem( final PSICompactItem parent, final String name, final ObjectInfo objectInfo )
  {
    if( objectInfo == null )
      throw new IllegalArgumentException("ObjectInfo null");
    
    m_parent = parent;
    m_name = name;
    m_objectInfo = objectInfo;
    
    m_children = new Vector();
  }
  
  /**
   * @see org.kalypso.util.repository.IRepositoryItem#getName()
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
   * @see org.kalypso.util.repository.IRepositoryItem#getParent()
   */
  public IRepositoryItem getParent()
  {
    return m_parent;
  }

  /**
   * @see org.kalypso.util.repository.IRepositoryItem#hasChildren()
   */
  public boolean hasChildren()
  {
    return getChildren().length != 0;
  }

  /**
   * @see org.kalypso.util.repository.IRepositoryItem#getChildren()
   */
  public IRepositoryItem[] getChildren()
  {
    return (IRepositoryItem[])m_children.toArray( new IRepositoryItem[ m_children.size()] );
  }

  /**
   * @see org.kalypso.ogc.sensor.ITarget#getSource()
   */
  public String getSource()
  {
    return "PSICompactItem";
  }

  /**
   * @see org.kalypso.ogc.sensor.ITarget#getType()
   */
  public String getType()
  {
    return getClass().getName();
  }

  /**
   * @see org.kalypso.ogc.sensor.ITarget#getIdentifier()
   */
  public String getIdentifier()
  {
    return m_objectInfo.getId();
  }
}
