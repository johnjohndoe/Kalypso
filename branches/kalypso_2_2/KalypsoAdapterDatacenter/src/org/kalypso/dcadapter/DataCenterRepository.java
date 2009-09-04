package org.kalypso.dcadapter;

import org.kalypso.repository.AbstractRepository;
import org.kalypso.repository.IRepositoryItem;

import com.bce.datacenter.db.IngresDatabase;
import com.bce.datacenter.db.common.Level;

/**
 * DataCenterRepository
 * 
 * @author marc
 */
public class DataCenterRepository extends AbstractRepository
{
  private final IngresDatabase m_database;

  private DataCenterLevelItem m_root;

  public DataCenterRepository( final String name, final String factory, final String conf, final boolean ro,
      final String url, final String userName, final String password )
  {
    super( name, factory, conf, ro, "datacenter://" );

    m_database = new IngresDatabase( url, userName, password );
  }

  /**
   * @see org.kalypso.repository.IRepository#getDescription()
   */
  @Override
  public String getDescription( )
  {
    return m_database.getUrl();
  }
  
  /**
   * @see org.kalypso.repository.IRepository#findItem(java.lang.String)
   */
  public IRepositoryItem findItem( final String id )
  {
    // TODO implement this
    return null;
  }

  /**
   * @see org.kalypso.repository.IRepository#reload()
   */
  public void reload( )
  {
    m_root = null;
  }


  private DataCenterLevelItem getRootItem( )
  {
    if( m_root == null )
      m_root = new DataCenterLevelItem( this, null, Level.getRoot( m_database
          .getConnection() ) );

    return m_root;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#hasChildren()
   */
  public boolean hasChildren( )
  {
    return getRootItem().hasChildren();
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getChildren()
   */
  public IRepositoryItem[] getChildren( )
  {
    return getRootItem().getChildren();
  }
}