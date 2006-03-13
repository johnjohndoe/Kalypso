/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.services.ocs.repository;

import java.util.List;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.repository.IRepository;
import org.kalypso.repository.IRepositoryItem;
import org.kalypso.repository.RepositoryException;
import org.kalypso.services.sensor.impl.ItemBean;
import org.kalypso.services.sensor.impl.KalypsoObservationService;
import org.kalypso.services.sensor.impl.ObservationBean;
import org.kalypso.services.sensor.impl.RepositoryException_Exception;
import org.kalypso.services.sensor.impl.SensorException_Exception;

/**
 * @author schlienger
 */
public class ServiceRepositoryItem implements IRepositoryItem
{
  private final ItemBean m_bean;

  private final ServiceRepositoryItem m_parent;

  private final KalypsoObservationService m_srv;

  private final IRepository m_rep;

  public ServiceRepositoryItem( final KalypsoObservationService srv, final ItemBean bean, final ServiceRepositoryItem parent, final IRepository rep )
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
    catch( final RepositoryException_Exception e )
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
      final List<ItemBean> beans = m_srv.getChildren( m_bean );

      final IRepositoryItem[] items = new ServiceRepositoryItem[beans.size()];

      for( int i = 0; i < items.length; i++ )
        items[i] = new ServiceRepositoryItem( m_srv, beans.get( i ), this, m_rep );

      return items;
    }
    catch( final RepositoryException_Exception e )
    {
      throw new RepositoryException( e );
    }
  }

  /**
   * @see org.eclipse.core.runtime.IAdaptable#getAdapter(java.lang.Class)
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
      catch( final SensorException_Exception e )
      {
        e.printStackTrace();
      }
    }

    return null;
  }

  @Override
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