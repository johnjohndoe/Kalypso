package org.kalypso.ui.repository.services;

import java.net.URL;
import java.rmi.RemoteException;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.zml.ZmlObservation;
import org.kalypso.repository.IRepositoryItem;
import org.kalypso.services.proxy.IObservationService;
import org.kalypso.services.proxy.ItemBean;
import org.kalypso.services.proxy.ObservationBean;
import org.kalypso.services.proxy.ObservationDataDescriptorBean;

/**
 * @author schlienger
 */
public class ServiceRepositoryItem implements IRepositoryItem
{
  private final ItemBean m_bean;
  private final ServiceRepositoryItem m_parent;
  private final IObservationService m_srv;

  public ServiceRepositoryItem( final IObservationService srv, final ItemBean bean, final ServiceRepositoryItem parent )
  {
    m_srv = srv;
    m_bean = bean;
    m_parent = parent;
  }
  
  /**
   * @see org.kalypso.repository.IRepositoryItem#getName()
   */
  public String getName()
  {
    return m_bean.getName();
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
    try
    {
      return m_srv.hasChildren( m_bean );
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
      final ItemBean[] beans = m_srv.getChildren( m_bean );
      
      final IRepositoryItem[] items = new ServiceRepositoryItem[ beans.length ];
      
      for( int i = 0; i < items.length; i++ )
        items[i] = new ServiceRepositoryItem( m_srv, beans[i], this );
      
      return items;
    }
    catch( RemoteException e )
    {
      e.printStackTrace();
      return null;
    }
  }

  /**
   * @see org.kalypso.util.adapter.IAdaptable#getAdapter(java.lang.Class)
   */
  public Object getAdapter( final Class anotherClass )
  {
    if( anotherClass == IObservation.class && m_bean instanceof ObservationBean )
    {
      final ObservationBean ob = (ObservationBean)m_bean;
      
      try
      {
        final ObservationDataDescriptorBean oddb = m_srv.readData( ob );
        
        final ZmlObservation obs = new ZmlObservation( new URL( oddb.getLocation() ) );
        
        m_srv.clearTempData( oddb );
        
        return obs;
      }
      catch( Exception e ) // generic exception caught for simplicity
      {
        e.printStackTrace();
        return null;
      }
    }
    
    return null;
  }
  
  /**
   * @see java.lang.Object#toString()
   */
  public String toString()
  {
    return getName();
  }
}
