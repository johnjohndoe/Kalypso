package org.kalypso.services.sensor.impl;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.rmi.RemoteException;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import org.kalypso.java.io.FileUtilities;
import org.kalypso.java.lang.reflect.ClassUtilities.ClassUtilityException;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.beans.ObservationBean;
import org.kalypso.ogc.sensor.beans.ObservationDataDescriptorBean;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.repository.IRepository;
import org.kalypso.repository.IRepositoryFactory;
import org.kalypso.repository.IRepositoryItem;
import org.kalypso.repository.RepositoryException;
import org.kalypso.repository.beans.ItemBean;
import org.kalypso.repository.conf.RepositoryConfig;
import org.kalypso.repository.conf.RepositoryConfigItem;
import org.kalypso.repository.conf.RepositoryConfigUtils;
import org.kalypso.services.common.ServiceConfig;
import org.kalypso.services.sensor.IObservationService;
import org.kalypso.zml.ObservationType;

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

  private final File m_tmpDir;

  /**
   * Constructs the service by reading the configuration.
   * 
   * @throws RepositoryException
   * @throws ClassUtilityException
   * @throws FileNotFoundException
   */
  public KalypsoObservationService() throws RepositoryException, ClassUtilityException, FileNotFoundException
  {
    final File conf = new File( ServiceConfig.getConfDir(), "IObservationService/repconf_server.xml" );
    final InputStream stream = new FileInputStream( conf );

    // this call also closes the stream
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
    
    m_tmpDir = FileUtilities.createNewTempDir( "Observations", ServiceConfig.getTempDir() );
  }

  /**
   * @see org.kalypso.services.sensor.IObservationService#readData(org.kalypso.ogc.sensor.beans.ObservationBean)
   */
  public ObservationDataDescriptorBean readData( final ObservationBean observation ) throws RemoteException
  {
    final IRepositoryItem item = (IRepositoryItem)m_mapId2Obj.get( new Integer( observation.getId() ) );
    final IObservation obs = (IObservation)item.getAdapter( IObservation.class );
    
    FileOutputStream fos = null;
    
    try
    {
      final ObservationType obsType = ZmlFactory.createXML( obs );
      
      final File f = File.createTempFile( obs.getName(), ".zml", m_tmpDir );
      
      // will be closed in finally block
      fos = new FileOutputStream( f );
      ZmlFactory.getMarshaller().marshal( obsType, fos );
      
      final ObservationDataDescriptorBean oddb = new ObservationDataDescriptorBean( m_lastId++, f.toURL().toString(), "zml" );

      // store the file for future use
      m_mapId2Obj.put( new Integer( oddb.getId()), f );
      
      return oddb;
    }
    catch( Exception e ) // generic exception used for simplicity
    {
      throw new RemoteException( "", e );
    }
    finally
    {
      if( fos != null )
        try
        {
          fos.close();
        }
        catch( IOException e2 )
        {
          throw new RemoteException( "Exception while closing output stream", e2 );
        }
    }
  }

  /**
   * @see org.kalypso.services.sensor.IObservationService#clearTempData(org.kalypso.ogc.sensor.beans.ObservationDataDescriptorBean)
   */
  public void clearTempData( final ObservationDataDescriptorBean bean )
  {
    final Integer id = new Integer( bean.getId() );
    
    if( m_mapId2Obj.containsKey( id ) )
    {
      final File f = (File)m_mapId2Obj.get( id );
      f.delete();
      
      m_mapId2Obj.remove( id );
    }
  }
  
  /**
   * @see org.kalypso.services.sensor.IObservationService#writeData(org.kalypso.ogc.sensor.beans.ObservationBean,
   *      org.kalypso.ogc.sensor.beans.ObservationDataDescriptorBean)
   */
  public void writeData( final ObservationBean observation, final ObservationDataDescriptorBean descriptor )
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
      // already in cache?
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

    // already in cache?
    if( m_mapObj2Obj.containsKey( item ) )
      return (ItemBean[])m_mapObj2Obj.get( item );

    final IRepositoryItem[] children = item.getChildren();
    final ItemBean[] beans = new ItemBean[children.length];

    for( int i = 0; i < beans.length; i++ )
    {
      final IObservation obs = (IObservation)children[i].getAdapter( IObservation.class );
      if( obs != null )
      {
        beans[i] = new ObservationBean( m_lastId++, obs.getName(), obs.getMetadataList() );
      }
      else
      {      
        beans[i] = new ItemBean( m_lastId++, children[i].getName() );
      }
      
      // store it for future referencing
      m_mapId2Obj.put( new Integer( beans[i].getId() ), children[i] );
    }

    // cache it for next request
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
   * @see org.kalypso.services.sensor.IObservationService#getDescription()
   */
  public String getDescription()
  {
    return System.getProperty( "Kalypso Zeitreihendienst" );
  }
}