package org.kalypso.services.ocs.repository;

import java.rmi.RemoteException;

import javax.xml.rpc.ServiceException;

import org.kalypso.repository.AbstractRepository;
import org.kalypso.repository.IRepositoryFactory;
import org.kalypso.repository.IRepositoryItem;
import org.kalypso.repository.RepositoryException;
import org.kalypso.services.proxy.IObservationService;
import org.kalypso.services.proxy.ItemBean;
import org.kalypso.ui.KalypsoGisPlugin;

/**
 * Repository of the Observation Service.
 * 
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
  public ObservationServiceRepository( final IRepositoryFactory factory, final boolean readOnly ) throws ServiceException
  {
    super( factory, "", readOnly );
    
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
  public boolean hasChildren() throws RepositoryException
  {
    try
    {
      return m_srv.hasChildren( ROOT_ITEM );
    }
    catch( RemoteException e )
    {
      throw new RepositoryException( e );
    }
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getChildren()
   */
  public IRepositoryItem[] getChildren() throws RepositoryException
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
      throw new RepositoryException( e );
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
  public IRepositoryItem findItem( final String id ) throws RepositoryException
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
  private IRepositoryItem findItemRecursive( final String id, final IRepositoryItem item ) throws RepositoryException
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