package org.kalypso.services.sensor;

import java.rmi.RemoteException;

import org.kalypso.ogc.sensor.beans.ObservationBean;
import org.kalypso.ogc.sensor.beans.ObservationDataDescriptorBean;
import org.kalypso.services.repository.IRepositoryService;

/**
 * WebService interface for the Observation-Service.
 * 
 * @author schlienger
 */
public interface IObservationService extends IRepositoryService
{
  /**
   * Returns some useful description that will be displayed to the user in order to identify this service.
   */
  public String getDescription() throws RemoteException;
  
  /**
   * Returns the service version
   */
  public int getServiceVersion() throws RemoteException;
  
  /**
   * Reads the data out. Does not return the data itself but a descriptor which
   * describes where the data is to be found.
   */
  public ObservationDataDescriptorBean readData( final ObservationBean observation ) throws RemoteException;

  /**
   * Call this method once client is done with manipulation of the data underlying
   * the given bean. The service will then free any resources hold by the bean.
   */
  public void clearTempData( final ObservationDataDescriptorBean bean ) throws RemoteException;
  
  /**
   * Writes the data in. Does not take the data as argument but a descriptor
   * which describes where the data is to be found.
   */
  public void writeData( final ObservationBean observation, final ObservationDataDescriptorBean descriptor ) throws RemoteException;
}