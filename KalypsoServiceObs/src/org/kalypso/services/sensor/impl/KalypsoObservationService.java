package org.kalypso.services.sensor.impl;

import java.io.InputStream;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import org.kalypso.java.lang.reflect.ClassUtilities.ClassUtilityException;
import org.kalypso.ogc.sensor.beans.ObservationBean;
import org.kalypso.ogc.sensor.beans.ObservationDescriptorBean;
import org.kalypso.repository.IRepository;
import org.kalypso.repository.IRepositoryFactory;
import org.kalypso.repository.RepositoryException;
import org.kalypso.repository.beans.ItemBean;
import org.kalypso.repository.beans.RepositoryBean;
import org.kalypso.repository.conf.RepositoryConfig;
import org.kalypso.repository.conf.RepositoryConfigItem;
import org.kalypso.repository.conf.RepositoryConfigUtils;
import org.kalypso.services.sensor.IObservationService;

/**
 * Kalypso Observation Service.
 * 
 * @author schlienger
 */
public class KalypsoObservationService implements IObservationService
{
  private final List m_repositories;

  /**
   * Constructs the service by reading the configuration.
   * 
   * @throws RepositoryException
   * @throws ClassUtilityException
   */
  public KalypsoObservationService() throws RepositoryException, ClassUtilityException
  {
    final InputStream stream = getClass().getResourceAsStream( "./resources/repconf_server.xml" );
    
    // stream will be closed after this call
    final RepositoryConfig config = RepositoryConfigUtils.loadConfig( stream );
    
    final List items = config.getItems();
    m_repositories = new Vector( items.size() );
    
    for( final Iterator it = items.iterator(); it.hasNext(); )
    {
      final RepositoryConfigItem item = (RepositoryConfigItem)it.next();
      final IRepositoryFactory fact = item.createFactory();
      
      m_repositories.add( fact.createRepository() );
    }
  }
  
  /**
   * @see org.kalypso.services.sensor.IObservationService#getObservations(org.kalypso.repository.beans.ItemBean)
   */
  public ObservationBean[] getObservations( ItemBean node )
  {
    return null;
  }

  /**
   * @see org.kalypso.services.sensor.IObservationService#getMetadataFor(org.kalypso.ogc.sensor.beans.ObservationBean)
   */
  public Map getMetadataFor( ObservationBean observation )
  {
    return null;
  }

  /**
   * @see org.kalypso.services.sensor.IObservationService#readData(org.kalypso.ogc.sensor.beans.ObservationBean)
   */
  public ObservationDescriptorBean readData( ObservationBean observation )
  {
    return null;
  }

  /**
   * @see org.kalypso.services.sensor.IObservationService#writeData(org.kalypso.ogc.sensor.beans.ObservationBean, org.kalypso.ogc.sensor.beans.ObservationDescriptorBean)
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
    final RepositoryBean[] beans = new RepositoryBean[ m_repositories.size() ];
    
    int i = 0;
    for( final Iterator it = m_repositories.iterator(); it.hasNext(); )
    {
      final IRepository rep = (IRepository)it.next();
      
      beans[i] = new RepositoryBean( i, rep.getName() );
      
      i++;
    }
    
    return beans;
  }

  /**
   * @see org.kalypso.services.repository.IRepositoryService#getRoots(org.kalypso.repository.beans.RepositoryBean)
   */
  public ItemBean[] getRoots( RepositoryBean repository )
  {
    return null;
  }

  /**
   * @see org.kalypso.services.repository.IRepositoryService#getChildren(org.kalypso.repository.beans.ItemBean)
   */
  public ItemBean[] getChildren( ItemBean parent )
  {
    return null;
  }
}
