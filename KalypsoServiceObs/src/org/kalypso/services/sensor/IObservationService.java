package org.kalypso.services.sensor;

import java.rmi.RemoteException;
import java.util.Map;

import org.kalypso.ogc.sensor.beans.ObservationBean;
import org.kalypso.ogc.sensor.beans.ObservationDescriptorBean;
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
   * Returns the service version
   */
  public int getServiceVersion() throws RemoteException;
  
  /**
   * Returns the list of Observations at the given node.
   */
  public ObservationBean[] getObservations( final ItemBean node ) throws RemoteException;

  /**
   * Returns the metadata-list for the given observation.
   */
  public Map getMetadataFor( final ObservationBean observation ) throws RemoteException;

  /**
   * Reads the data out. Does not return the data itself but a descriptor which
   * describes where the data is to be found.
   */
  public ObservationDescriptorBean readData( final ObservationBean observation ) throws RemoteException;

  /**
   * Writes the data in. Does not take the data as argument but a descriptor
   * which describes where the data is to be found.
   */
  public void writeData( final ObservationBean observation, final ObservationDescriptorBean descriptor ) throws RemoteException;
  
  
}