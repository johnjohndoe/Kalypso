package org.kalypso.services.sensor.impl;

import java.io.InputStream;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import org.kalypso.java.lang.reflect.ClassUtilities.ClassUtilityException;
import org.kalypso.ogc.sensor.beans.ObservationBean;
import org.kalypso.ogc.sensor.beans.ObservationDataDescriptorBean;
import org.kalypso.repository.IRepository;
import org.kalypso.repository.IRepositoryFactory;
import org.kalypso.repository.IRepositoryItem;
import org.kalypso.repository.RepositoryException;
import org.kalypso.repository.beans.ItemBean;
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

  private final Map m_mapId2Obj;

  private final Map m_mapObj2Obj;

  private int m_lastId;

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

    m_mapId2Obj = new Hashtable( 512 );
    m_mapObj2Obj = new Hashtable( 512 );

    m_lastId = 0;
  }

  /**
   * @see org.kalypso.services.sensor.IObservationService#readData(org.kalypso.ogc.sensor.beans.ObservationBean)
   */
  public ObservationDataDescriptorBean readData( ObservationBean observation )
  {
    return null;
  }

  /**
   * @see org.kalypso.services.sensor.IObservationService#writeData(org.kalypso.ogc.sensor.beans.ObservationBean,
   *      org.kalypso.ogc.sensor.beans.ObservationDataDescriptorBean)
   */
  public void writeData( ObservationBean observation, ObservationDataDescriptorBean descriptor )
  {
  //
  }

  /**
   * @see org.kalypso.services.repository.IRepositoryService#hasChildren(org.kalypso.repository.beans.ItemBean)
   */
  public boolean hasChildren( final ItemBean parent )
  {
    // dealing with ROOT?
    if( parent == null )
      return m_repositories.size() > 0;

    final IRepositoryItem item = (IRepositoryItem)m_mapId2Obj.get( new Integer( parent.getId() ) );
    return item.hasChildren();
  }

  /**
   * @see org.kalypso.services.repository.IRepositoryService#getChildren(org.kalypso.repository.beans.ItemBean)
   */
  public ItemBean[] getChildren( final ItemBean parent )
  {
    // dealing with ROOT?
    if( parent == null )
    {
      if( m_mapObj2Obj.containsKey( m_repositories ) )
        return (ItemBean[])m_mapObj2Obj.get( m_repositories );

      final ItemBean[] beans = new ItemBean[m_repositories.size()];

      for( int i = 0; i < beans.length; i++ )
      {
        final IRepository rep = (IRepository)m_repositories.get( i );

        beans[i] = new ItemBean( m_lastId++, rep.getName() );
        m_mapId2Obj.put( new Integer( beans[i].getId() ), rep );
      }

      m_mapObj2Obj.put( m_repositories, beans );

      return beans;
    }

    final IRepositoryItem item = (IRepositoryItem)m_mapId2Obj.get( new Integer( parent.getId() ) );

    if( m_mapObj2Obj.containsKey( item ) )
      return (ItemBean[])m_mapObj2Obj.get( item );

    final IRepositoryItem[] children = item.getChildren();
    final ItemBean[] beans = new ItemBean[children.length];

    for( int i = 0; i < beans.length; i++ )
    {
      beans[i] = new ItemBean( m_lastId++, children[i].getName() );
      m_mapId2Obj.put( new Integer( beans[i].getId() ), children[i] );
    }

    m_mapObj2Obj.put( item, beans );

    return beans;
  }

  /**
   * @see org.kalypso.services.sensor.IObservationService#getServiceVersion()
   */
  public int getServiceVersion()
  {
    return 0;
  }

  /**
   * @see org.kalypso.services.sensor.IObservationService#hasObservations(org.kalypso.repository.beans.ItemBean)
   */
  public boolean hasObservations( final ItemBean node )
  {
    return false;
  }

  /**
   * @see org.kalypso.services.sensor.IObservationService#getDescription()
   */
  public String getDescription()
  {
    // TODO way to specify location?
    return System.getProperty( "Kalypso Observation Service" );
  }
}