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

import java.rmi.RemoteException;
import java.util.HashMap;
import java.util.Map;

import javax.xml.rpc.ServiceException;

import org.kalypso.repository.AbstractRepository;
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
  private final static ItemBean ROOT_ITEM = null;

  private final IObservationService m_srv;

  private final Map m_foundItems = new HashMap();

  /**
   * @throws ServiceException
   *           when the underlying service is not available
   */
  public ObservationServiceRepository( String name, String factory, boolean readOnly ) throws ServiceException
  {
    super( name, factory, "", readOnly );

    m_srv = KalypsoGisPlugin.getDefault().getObservationServiceProxy();
  }

  /**
   * @see org.kalypso.repository.AbstractRepository#getDescription()
   */
  public String getDescription()
  {
    try
    {
      return m_srv.getDescription();
    }
    catch( final RemoteException e )
    {
      e.printStackTrace();

      return "<Fehler...>";
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

      final IRepositoryItem[] items = new IRepositoryItem[beans.length];

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
    m_foundItems.clear();
    
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

    return item;
  }

  /**
   * Helper: finds item using recursion
   * 
   * @return item if found, otherwise null.
   */
  private IRepositoryItem findItemRecursive( final String id, final IRepositoryItem item ) throws RepositoryException
  {
    IRepositoryItem foundItem = null;

    // first lookup in the cache
    foundItem = (IRepositoryItem)m_foundItems.get( id );
    if( foundItem != null )
      return foundItem;

    // either this is the item, or find recursive
    if( item.getIdentifier().equalsIgnoreCase( id ) )
      foundItem = item;
    else
    {
      final IRepositoryItem[] items = item.getChildren();
      for( int i = 0; i < items.length; i++ )
      {
        foundItem = findItemRecursive( id, items[i] );

        if( foundItem != null )
          break;
      }
    }

    if( foundItem != null )
      m_foundItems.put( id, foundItem );
    
    return foundItem;
  }
}