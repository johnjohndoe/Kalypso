package org.kalypso.services.sensor;

import java.rmi.RemoteException;

import org.kalypso.ogc.sensor.beans.ObservationBean;
import org.kalypso.ogc.sensor.beans.ObservationDataDescriptorBean;
import org.kalypso.repository.beans.ItemBean;
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
   * Returns true when the given node contains Observations.
   */
  public boolean hasObservations( final ItemBean node ) throws RemoteException;
  
  /**
   * Reads the data out. Does not return the data itself but a descriptor which
   * describes where the data is to be found.
   */
  public ObservationDataDescriptorBean readData( final ObservationBean observation ) throws RemoteException;

  /**
   * Writes the data in. Does not take the data as argument but a descriptor
   * which describes where the data is to be found.
   */
  public void writeData( final ObservationBean observation, final ObservationDataDescriptorBean descriptor ) throws RemoteException;
}