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
package org.kalypso.services.sensor;

import java.rmi.RemoteException;

import org.kalypso.ogc.sensor.beans.DateRangeBean;
import org.kalypso.ogc.sensor.beans.OCSDataBean;
import org.kalypso.ogc.sensor.beans.ObservationBean;
import org.kalypso.repository.beans.ItemBean;
import org.kalypso.services.IKalypsoService;
import org.kalypso.services.repository.IRepositoryService;

/**
 * WebService interface for the Observation-Service.
 * 
 * @author schlienger
 */
public interface IObservationService extends IRepositoryService, IKalypsoService
{
  /**
   * @return useful (short) description that will be displayed to the user in order to identify this service.
   * @throws RemoteException
   */
  public String getDescription() throws RemoteException;
  
  /**
   * @param ib
   * @return observation bean if itembean is adaptable to a IObservation. Returns null otherwise.
   * @throws RemoteException
   */
  public ObservationBean adaptItem( final ItemBean ib ) throws RemoteException;
  
  /**
   * Reads the data out. Does not return the data itself but a descriptor which
   * describes where the data is to be found.
   * @param observation
   * @param drb
   * @return data bean
   * @throws RemoteException
   */
  public OCSDataBean readData( final ObservationBean observation, final DateRangeBean drb ) throws RemoteException;

  /**
   * Call this method once client is done with manipulation of the data underlying
   * the given bean. The service will then free any resources hold by the bean.
   * @param bean
   * @throws RemoteException
   */
  public void clearTempData( final OCSDataBean bean ) throws RemoteException;
  
  /**
   * Writes the data in. Does not take the data as argument but a descriptor
   * which describes where the data is to be found.
   * @param observation
   * @param descriptor
   * @throws RemoteException
   */
  public void writeData( final ObservationBean observation, final OCSDataBean descriptor ) throws RemoteException;
  
  /**
   * Prepares a container on the server side so that client can write data into it.
   * 
   * @param obs
   * @return data bean where client can safely write data into
   * @throws RemoteException
   */
  public OCSDataBean prepareForWrite( final ObservationBean obs ) throws RemoteException;
}