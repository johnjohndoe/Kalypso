package org.kalypso.services.ocs.repository;

import java.rmi.RemoteException;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.repository.IRepository;
import org.kalypso.repository.IRepositoryItem;
import org.kalypso.repository.RepositoryException;
import org.kalypso.services.proxy.IObservationService;
import org.kalypso.services.proxy.ItemBean;
import org.kalypso.services.proxy.ObservationBean;

/**
 * @author schlienger
 */
public class ServiceRepositoryItem implements IRepositoryItem
{
  private final ItemBean m_bean;

  private final ServiceRepositoryItem m_parent;

  private final IObservationService m_srv;

  private final IRepository m_rep;

  public ServiceRepositoryItem( final IObservationService srv,
      final ItemBean bean, final ServiceRepositoryItem parent,
      final IRepository rep )
  {
    m_rep = rep;
    m_srv = srv;
    m_bean = bean;
    m_parent = parent;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getName()
   */
  public String getName( )
  {
    return m_bean.getName();
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getParent()
   */
  public IRepositoryItem getParent( )
  {
    return m_parent;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#hasChildren()
   */
  public boolean hasChildren( ) throws RepositoryException
  {
    try
    {
      return m_srv.hasChildren( m_bean );
    }
    catch( RemoteException e )
    {
      throw new RepositoryException( e );
    }
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getChildren()
   */
  public IRepositoryItem[] getChildren( ) throws RepositoryException
  {
    try
    {
      final ItemBean[] beans = m_srv.getChildren( m_bean );

      final IRepositoryItem[] items = new ServiceRepositoryItem[beans.length];

      for( int i = 0; i < items.length; i++ )
        items[i] = new ServiceRepositoryItem( m_srv, beans[i], this, m_rep );

      return items;
    }
    catch( RemoteException e )
    {
      throw new RepositoryException( e );
    }
  }

  /**
   * @see org.kalypso.util.adapter.IAdaptable#getAdapter(java.lang.Class)
   */
  public Object getAdapter( final Class anotherClass )
  {
    if( anotherClass == IObservation.class )
    {
      try
      {
        final ObservationBean bean = m_srv.adaptItem( m_bean );

        if( bean == null )
          return null;

        return new ServiceRepositoryObservation( m_srv, bean );
      }
      catch( RemoteException e )
      {
        e.printStackTrace();
      }
    }

    return null;
  }

  /**
   * @see java.lang.Object#toString()
   */
  public String toString( )
  {
    return getName();
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getIdentifier()
   */
  public String getIdentifier( )
  {
    return m_bean.getId();
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getRepository()
   */
  public IRepository getRepository( )
  {
    return m_rep;
  }
}