package org.kalypso.psiadapter.repository;

import java.util.List;
import java.util.Vector;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.psiadapter.PSICompactFactory;
import org.kalypso.repository.IRepository;
import org.kalypso.repository.IRepositoryItem;
import org.kalypso.repository.RepositoryException;

import de.psi.go.lhwz.ECommException;
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

  private final String m_identifier;

  private final List m_children;

  protected final ObjectInfo m_objectInfo;

  private final int m_valueType;

  /**
   * Constructor
   * 
   * @param parent
   * @param name
   * @param identifier
   * @param info
   * @param valueType
   */
  public PSICompactItem( final PSICompactItem parent, final String name, final String identifier,
      final PSICompact.ObjectInfo info, final int valueType )
  {
    m_parent = parent;
    m_name = name;
    m_identifier = identifier;
    m_objectInfo = info;
    m_valueType = valueType;

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

  /**
   * @param item
   */
  public void addChild( final PSICompactItem item )
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
    return (IRepositoryItem[])m_children.toArray( new IRepositoryItem[m_children.size()] );
  }

  /**
   * @see org.kalypso.util.adapter.IAdaptable#getAdapter(java.lang.Class)
   */
  public Object getAdapter( final Class anotherClass )
  {
    try
    {
      final boolean adaptable = PSICompactFactory.getConnection().getMeasureType( m_identifier ) != PSICompact.TYPE_UNDEF;

      if( adaptable && anotherClass == IObservation.class )
        return new PSICompactObservationItem( getName(), getIdentifier(), m_objectInfo, m_valueType );
    }
    catch( ECommException e )
    {
      e.printStackTrace();

      return null;
    }

    return null;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getRepository()
   */
  public IRepository getRepository()
  {
    try
    {
      return PSICompactRepositoryFactory.getRepository();
    }
    catch( RepositoryException e )
    {
      e.printStackTrace();

      throw new IllegalStateException( "Invalid repository. See previous stack trace for the RepositoryException" );
    }
  }

  /**
   * Returns
   * 
   * <pre>
   * psicompact://psi...id
   * </pre>
   * 
   * with psi...id being the id that is delivered from the PSICompact interface.
   * 
   * @see org.kalypso.repository.IRepositoryItem#getIdentifier()
   */
  public String getIdentifier()
  {
    return getRepository().getIdentifier() + m_identifier;
  }
}