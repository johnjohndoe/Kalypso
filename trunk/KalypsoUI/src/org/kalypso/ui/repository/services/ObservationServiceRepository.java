package org.kalypso.ui.repository.services;

import java.rmi.RemoteException;

import javax.xml.rpc.ServiceException;

import org.kalypso.repository.AbstractRepository;
import org.kalypso.repository.IRepositoryItem;
import org.kalypso.services.proxy.IObservationService;
import org.kalypso.services.proxy.RepositoryBean;
import org.kalypso.ui.KalypsoGisPlugin;

/**
 * @author schlienger
 */
public class ObservationServiceRepository extends AbstractRepository
{
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
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#hasChildren()
   */
  public boolean hasChildren()
  {
    try
    {
      return m_srv.hasRepositories();
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
      final RepositoryBean[] beans = m_srv.getRepositories();
      
      final IRepositoryItem[] items = new IRepositoryItem[ beans.length ];
      
      for( int i = 0; i < items.length; i++ )
        items[i] = new ServiceRepositoryItem( m_srv, beans[i], null );
      
      return items;
    }
    catch( RemoteException e )
    {
      e.printStackTrace();
      return null;
    }
  }
}
