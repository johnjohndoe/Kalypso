package org.kalypso.ui.repository.services;

import java.rmi.RemoteException;

import javax.xml.rpc.ServiceException;

import org.kalypso.repository.AbstractRepository;
import org.kalypso.repository.IRepositoryItem;
import org.kalypso.repository.RepositoryException;
import org.kalypso.services.proxy.IObservationService;
import org.kalypso.services.proxy.ItemBean;
import org.kalypso.ui.KalypsoGisPlugin;

/**
 * @author schlienger
 */
public class ObservationServiceRepository extends AbstractRepository
{
  /** root item is identified by the null bean */
  private final ItemBean ROOT_ITEM = null;
  
  private final IObservationService m_srv;

  /**
   * Constructor
   * 
   * @throws ServiceException when the underlying service is not available
   */
  public ObservationServiceRepository( ) throws ServiceException
  {
    super();
    
    m_srv = KalypsoGisPlugin.getDefault().getObservationServiceProxy();
    
    try
    {
      m_location = m_srv.getDescription();
    }
    catch( RemoteException e )
    {
      throw new ServiceException( e );
    }
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#hasChildren()
   */
  public boolean hasChildren()
  {
    try
    {
      return m_srv.hasChildren( ROOT_ITEM );
    }
    catch( RemoteException e )
    {
      e.printStackTrace();
      return false;
    }
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getChildren()
   */
  public IRepositoryItem[] getChildren()
  {
    try
    {
      final ItemBean[] beans = m_srv.getChildren( ROOT_ITEM );
      
      final IRepositoryItem[] items = new IRepositoryItem[ beans.length ];
      
      for( int i = 0; i < items.length; i++ )
        items[i] = new ServiceRepositoryItem( m_srv, beans[i], null, this );
      
      return items;
    }
    catch( RemoteException e )
    {
      // TODO modify interface IRepositoryItem to throw a RepositoryException
      e.printStackTrace();
      return null;
    }
  }

  /**
   * @see org.kalypso.repository.IRepository#getIdentifier()
   */
  public String getIdentifier()
  {
    return "observation-service-repository";
  }

  /**
   * @see org.kalypso.repository.IRepository#reload()
   */
  public void reload() throws RepositoryException
  {
    try
    {
      m_srv.reload();
    }
    catch( RemoteException e )
    {
      throw new RepositoryException( e );
    }
  }

  /**
   * @see org.kalypso.repository.IRepository#findItem(java.lang.String)
   */
  public IRepositoryItem findItem( String id ) throws RepositoryException
  {
    final IRepositoryItem item = findItemRecursive( id, this );
    
    if( item == null )
      throw new RepositoryException( "Coult not find item: " + id );
    
    return item;
  }

  /**
   * Helper: finds item using recursion
   * TODO: better performance by caching items that were already found? do not forget
   * to clear the cache in reload()
   */
  private IRepositoryItem findItemRecursive( final String id, final IRepositoryItem item )
  {
    if( item.getIdentifier().equalsIgnoreCase( id ) )
      return item;
    
    final IRepositoryItem[] items = item.getChildren();
    for( int i = 0; i < items.length; i++ )
    {
      final IRepositoryItem item2 = findItemRecursive( id, items[i] );
      
      if( item2 != null )
        return item2;
    }
    
    return null;
  }
}