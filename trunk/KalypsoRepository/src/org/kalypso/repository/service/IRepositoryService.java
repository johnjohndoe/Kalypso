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
package org.kalypso.repository.service;

import java.rmi.Remote;
import java.rmi.RemoteException;



/**
 * General service base interface for repositories.
 * <p>
 * <b>IMPORTANT NOTE</b>: this interface is primary not intended to be directly
 * used as a webservice. It should be extended by some specific interfaces
 * that, in turn, are eligible to be real web services.
 * <p>
 * For an example see <code>IObservationService</code>.
 * 
 * @author schlienger
 */
public interface IRepositoryService extends Remote
{
  /**
   * @param parent
   * @return true if the given parent has children.
   * @throws RemoteException
   */
  public boolean hasChildren( final ItemBean parent ) throws RemoteException;
  
  /**
   * @param parent
   * @return the children of the given item (parent node). Returns an empty array
   * when the parent has no children.
   * @throws RemoteException
   */
  public ItemBean[] getChildren( final ItemBean parent ) throws RemoteException;
  
  /**
   * @param id
   * @return ItemBean if found, else null.
   * @throws RemoteException
   */
  public ItemBean findItem( final String id ) throws RemoteException;
  
  /**
   * Forces the refresh of the remote repository.
   * @throws RemoteException
   */
  public void reload() throws RemoteException;
}
