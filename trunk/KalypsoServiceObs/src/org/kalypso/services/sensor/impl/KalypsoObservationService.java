package org.kalypso.services.sensor.impl;

import java.util.Map;

import org.kalypso.services.repository.beans.ItemBean;
import org.kalypso.services.repository.beans.RepositoryBean;
import org.kalypso.services.sensor.IObservationService;
import org.kalypso.services.sensor.beans.ObservationBean;
import org.kalypso.services.sensor.beans.ObservationDescriptorBean;

/**
 * @author schlienger
 */
public class KalypsoObservationService implements IObservationService
{
  /**
   * @see org.kalypso.services.sensor.IObservationService#getObservations(org.kalypso.services.repository.beans.ItemBean)
   */
  public ObservationBean[] getObservations( ItemBean node )
  {
    return null;
  }

  /**
   * @see org.kalypso.services.sensor.IObservationService#getMetadataFor(org.kalypso.services.sensor.beans.ObservationBean)
   */
  public Map getMetadataFor( ObservationBean observation )
  {
    return null;
  }

  /**
   * @see org.kalypso.services.sensor.IObservationService#readData(org.kalypso.services.sensor.beans.ObservationBean)
   */
  public ObservationDescriptorBean readData( ObservationBean observation )
  {
    return null;
  }

  /**
   * @see org.kalypso.services.sensor.IObservationService#writeData(org.kalypso.services.sensor.beans.ObservationBean, org.kalypso.services.sensor.beans.ObservationDescriptorBean)
   */
  public void writeData( ObservationBean observation, ObservationDescriptorBean descriptor )
  {
    //
    }

  /**
   * @see org.kalypso.services.repository.IRepositoryService#getRepositories()
   */
  public RepositoryBean[] getRepositories()
  {
    return null;
  }

  /**
   * @see org.kalypso.services.repository.IRepositoryService#getRoots(org.kalypso.services.repository.beans.RepositoryBean)
   */
  public ItemBean[] getRoots( RepositoryBean repository )
  {
    return null;
  }

  /**
   * @see org.kalypso.services.repository.IRepositoryService#getChildren(org.kalypso.services.repository.beans.ItemBean)
   */
  public ItemBean[] getChildren( ItemBean parent )
  {
    return null;
  }
}
