package org.kalypso.dcadapter;

import org.kalypso.repository.AbstractRepository;
import org.kalypso.repository.IRepositoryItem;
import org.kalypso.repository.RepositoryException;

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

  public DataCenterRepository( String name, String factory, String conf, boolean ro,
      String url, String userName, String password )
  {
    super( name, factory, conf, ro );

    m_database = new IngresDatabase( url, userName, password );
  }

  /**
   * @see org.kalypso.repository.IRepository#getDescription()
   */
  public String getDescription( )
  {
    return m_database.getUrl();
  }
  
  /**
   * @see org.kalypso.repository.IRepository#findItem(java.lang.String)
   */
  public IRepositoryItem findItem( final String id ) throws RepositoryException
  {
    // TODO implement this
    return null;
  }

  /**
   * @see org.kalypso.repository.IRepository#reload()
   */
  public void reload( ) throws RepositoryException
  {
    m_root = null;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getIdentifier()
   */
  public String getIdentifier( )
  {
    return "datacenter://";
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
  public boolean hasChildren( ) throws RepositoryException
  {
    return getRootItem().hasChildren();
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getChildren()
   */
  public IRepositoryItem[] getChildren( ) throws RepositoryException
  {
    return getRootItem().getChildren();
  }
}