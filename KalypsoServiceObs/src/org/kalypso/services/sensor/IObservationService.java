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