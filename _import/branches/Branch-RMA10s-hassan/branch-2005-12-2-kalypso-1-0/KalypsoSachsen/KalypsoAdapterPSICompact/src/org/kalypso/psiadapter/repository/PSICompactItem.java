package org.kalypso.psiadapter.repository;

import java.util.List;
import java.util.Vector;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.psiadapter.PSICompactFactory;
import org.kalypso.repository.IRepository;
import org.kalypso.repository.IRepositoryItem;

import de.psi.go.lhwz.PSICompact;
import de.psi.go.lhwz.PSICompact.ObjectInfo;
import de.psi.go.lhwz.PSICompact.ObjectMetaData;

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

  private final String m_obsName;

  private final ObjectMetaData m_objectMetaData;

  private final int m_arcType;

  private final PSICompactRepository m_repository;

  private final boolean m_useAt;

  /**
   * @param objectMetaData The object metadata as, got from {@link PSICompact#getObjectMetaData(java.lang.String)} for the <code>identifier</code>
   * @param arcType One of the {@link PSICompact#ARC_DAY} constants.
   * @param useAt
   */
  public PSICompactItem( final PSICompactRepository repository, final PSICompactItem parent, final String name, final String identifier,
      final PSICompact.ObjectInfo info, final int valueType, final String obsName, final ObjectMetaData objectMetaData, final int arcType, boolean useAt )
  {
    m_repository = repository;
    m_parent = parent;
    m_name = name;
    m_identifier = identifier;
    m_objectInfo = info;
    m_valueType = valueType;
    m_obsName = obsName;
    m_objectMetaData = objectMetaData;
    m_arcType = arcType;
    m_useAt = useAt;

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

  public Object getAdapter( final Class anotherClass )
  {
    try
    {
      final boolean adaptable = PSICompactFactory.getConnection().getMeasureType( m_objectInfo.getId() ) != PSICompact.TYPE_UNDEF;

      if( adaptable && anotherClass == IObservation.class )
        return new PSICompactObservationItem( m_obsName, m_identifier, m_objectInfo, m_valueType, m_objectMetaData, m_arcType, m_useAt );
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      throw new IllegalStateException( e.getLocalizedMessage() );
    }

    return null;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getRepository()
   */
  public IRepository getRepository()
  {
      return m_repository;
  }

  /**
   * Returns
   * 
   * <pre>
   * psicompact://psi...id#ARCTYPE
   * </pre>
   * 
   * with psi...id being the id that is delivered from the PSICompact interface.
   * 
   * @see org.kalypso.repository.IRepositoryItem#getIdentifier()
   */
  public String getIdentifier()
  {
    return PSICompactRepository.IDENTIFIER + m_identifier;
  }
}