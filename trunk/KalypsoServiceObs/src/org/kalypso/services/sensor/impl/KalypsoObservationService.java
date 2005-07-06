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
package org.kalypso.services.sensor.impl;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.rmi.RemoteException;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.TimeZone;
import java.util.Vector;
import java.util.logging.FileHandler;
import java.util.logging.Logger;

import javax.activation.DataHandler;
import javax.activation.FileDataSource;

import org.apache.commons.io.IOUtils;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.java.lang.reflect.ClassUtilities;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.filter.FilterFactory;
import org.kalypso.ogc.sensor.filter.filters.ZmlFilter;
import org.kalypso.ogc.sensor.manipulator.IObservationManipulator;
import org.kalypso.ogc.sensor.request.IRequest;
import org.kalypso.ogc.sensor.request.ObservationRequest;
import org.kalypso.ogc.sensor.request.RequestFactory;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.ogc.sensor.zml.ZmlURL;
import org.kalypso.ogc.sensor.zml.ZmlURLConstants;
import org.kalypso.repository.IRepository;
import org.kalypso.repository.IRepositoryItem;
import org.kalypso.repository.RepositoryException;
import org.kalypso.repository.RepositoryUtils;
import org.kalypso.repository.conf.RepositoryConfigUtils;
import org.kalypso.repository.conf.RepositoryFactoryConfig;
import org.kalypso.repository.factory.IRepositoryFactory;
import org.kalypso.repository.service.ItemBean;
import org.kalypso.repository.service.RepositoryBean;
import org.kalypso.services.common.ServiceConfig;
import org.kalypso.services.sensor.DataBean;
import org.kalypso.services.sensor.IObservationService;
import org.kalypso.services.sensor.ObservationBean;
import org.kalypso.zml.ObservationType;
import org.kalypso.zml.request.RequestType;
import org.xml.sax.InputSource;

/**
 * Kalypso Observation Service.
 * 
 * @author schlienger
 */
public class KalypsoObservationService implements IObservationService
{
  private final List m_repositories;

  private ItemBean[] m_repositoryBeans = null;

  /** Bean-ID(String) --> IRepositoryItem */
  private final Map m_mapBeanId2Item;

  /** IRepositoryItem --> ItemBean */
  private final Map m_mapItem2Bean;

  /** Repository-ID(String) --> IRepository */
  private final Map m_mapRepId2Rep;

  /** Repository-ID(String) --> IObservationManipulator */
  private final Map m_mapRepId2Manip;

  /** Data-ID(String) --> File */
  private final Map m_mapDataId2File;

  private final File m_tmpDir;

  private final Logger m_logger;

  /** Timezone is used to convert dates between repositories and clients */
  private TimeZone m_timezone = null;

  /**
   * Constructs the service by reading the configuration.
   * 
   * @throws RemoteException
   */
  public KalypsoObservationService() throws RemoteException
  {
    m_repositories = new Vector();
    m_mapBeanId2Item = new Hashtable( 512 );
    m_mapItem2Bean = new Hashtable( 512 );
    m_mapRepId2Rep = new Hashtable();
    m_mapRepId2Manip = new Hashtable();
    m_mapDataId2File = new Hashtable( 128 );

    m_logger = Logger.getLogger( KalypsoObservationService.class.getName() );

    try
    {
      m_logger.addHandler( new FileHandler( ServiceConfig.getTempDir() + "/IObservation%g.log", 10000000, 1, true ) );
    }
    catch( Exception e ) // generic Exception caught for simplicity
    {
      e.printStackTrace();
      throw new RemoteException( "Exception in constructor von: " + getClass().getName(), e );
    }

    m_tmpDir = FileUtilities.createNewTempDir( "Observations", ServiceConfig.getTempDir() );
    m_tmpDir.deleteOnExit();

    init();
  }

  protected void finalize() throws Throwable
  {
    clearCache();

    // force delete, even if we called deleteOnExit()
    m_tmpDir.delete();
  }

  private void clearCache()
  {
    m_mapBeanId2Item.clear();
    m_mapItem2Bean.clear();
    m_mapRepId2Rep.clear();
    m_mapRepId2Manip.clear();
    m_repositoryBeans = null;

    // dispose repositories
    for( final Iterator it = m_repositories.iterator(); it.hasNext(); )
      ( (IRepository)it.next() ).dispose();
    m_repositories.clear();

    // clear temp files
    for( final Iterator it = m_mapDataId2File.values().iterator(); it.hasNext(); )
      ( (File)it.next() ).delete();
    m_mapDataId2File.clear();

    ZmlFilter.configureFor( null );
  }

  /**
   * Initialize the Service according to configuration.
   * 
   * @throws RemoteException
   */
  private final void init() throws RemoteException
  {
    clearCache();

    try
    {
      final File conf = new File( ServiceConfig.getConfDir(), "IObservationService/repconf_server.xml" );
      final InputStream stream = new FileInputStream( conf );
      // this call also closes the stream
      final List facConfs = RepositoryConfigUtils.loadConfig( stream );

      // load the service properties
      final Properties props = new Properties();
      final File fProps = new File( ServiceConfig.getConfDir(), "IObservationService/service.properties" );
      FileInputStream ins = null;
      try
      {
        ins = new FileInputStream( fProps );
        props.load( ins );
        ins.close();
      }
      catch( final IOException e )
      {
        m_logger.warning( "Cannot read properties-file: " + e.getLocalizedMessage() );
      }
      finally
      {
        IOUtils.closeQuietly( ins );
      }

      for( final Iterator it = facConfs.iterator(); it.hasNext(); )
      {
        final RepositoryFactoryConfig item = (RepositoryFactoryConfig)it.next();
        final IRepositoryFactory fact = item.createFactory( getClass().getClassLoader() );

        final IRepository rep = fact.createRepository();
        m_repositories.add( rep );

        m_mapRepId2Rep.put( rep.getIdentifier(), rep );

        // look into properties if an IObservationManipulator should be
        // configured for the current repository
        final String pManip = "MANIPULATOR_" + rep.getIdentifier();
        final String cnManip = props.getProperty( pManip );
        if( cnManip != null )
        {
          final IObservationManipulator man = (IObservationManipulator)ClassUtilities.newInstance( cnManip,
              IObservationManipulator.class, getClass().getClassLoader() );
          m_mapRepId2Manip.put( rep.getIdentifier(), man );
        }
      }

      // tricky: set the list of repositories to the ZmlFilter so that
      // it can directly fetch the observations without using the default
      // URL resolving stuff
      ZmlFilter.configureFor( m_repositories );

      // set the timezone according to the properties
      final String tzName = props.getProperty( "TIMEZONE_NAME" );
      if( tzName != null )
      {
        m_timezone = TimeZone.getTimeZone( tzName );
        m_logger.info( "TimeZone set on " + m_timezone );
      }
      else
      {
        m_timezone = null;
        m_logger.info( "Reset TimeZone. Name not found: " + tzName );
      }
    }
    catch( final Exception e ) // generic exception caught for simplicity
    {
      m_logger.throwing( getClass().getName(), "init", e );

      throw new RemoteException( "Exception in KalypsoObservationService.init()", e );
    }
  }

  public DataBean readData( final String href ) throws RemoteException
  {
    final String hereHref = ZmlURL.removeServerSideId( href );
    final String obsId = ZmlURL.getIdentifierPart( hereHref );
    final ObservationBean obean = new ObservationBean( obsId );

    FileOutputStream fos = null;

    try
    {
      IObservation obs = null;
      try
      {
        final IRepositoryItem item = itemFromBean( obean );

        obs = (IObservation)item.getAdapter( IObservation.class );
      }
      catch( final Exception e )
      {
        m_logger.info( "No observation for " + obean.getId() + ". Reason: " + e.getLocalizedMessage() );
      }
      finally
      {
        if( obs == null )
        {
          m_logger.info( "Trying to create default observation based on request..." );

          obs = RequestFactory.createDefaultObservation( hereHref );
        }
      }

      // request part specified?
      IRequest request = null;
      
      final RequestType requestType = RequestFactory.parseRequest( hereHref );
      if( requestType != null)
      {
        request = ObservationRequest.createWith( requestType );
      	
        m_logger.info( "Reading data for observation: " + obs.getName() + " Request: " + request );
      }
      else
        m_logger.info( "Reading data for observation: " + obs.getName() );
        
      updateObservation( obs, obean.getId() );

      // tricky: maybe make a filtered observation out of this one
      obs = FilterFactory.createFilterFrom( hereHref, obs, null );

      final ObservationType obsType = ZmlFactory.createXML( obs, request, m_timezone );

      final File f = File.createTempFile( "___" + obs.getName(), ".zml", m_tmpDir );

      // we say delete on exit even if we allow the client to delete the file
      // explicitely in the clearTempData() service call. This allows us to
      // clear temp files on shutdown in the case the client forgets it.
      f.deleteOnExit();

      fos = new FileOutputStream( f );
      ZmlFactory.getMarshaller().marshal( obsType, fos );
      fos.close();

      final DataBean data = new DataBean( f.toString(), new DataHandler( new FileDataSource( f ) ) );
      m_mapDataId2File.put( data.getId(), f );

      return data;
    }
    catch( final Exception e ) // generic exception used for simplicity
    {
      m_logger.throwing( getClass().getName(), "readData", e );
      throw new RemoteException( "", e );
    }
    finally
    {
      if( fos != null )
        try
        {
          fos.close();
        }
        catch( IOException e )
        {
          m_logger.severe( e.getLocalizedMessage() );
          throw new RemoteException( "Error closing the output stream", e );
        }
    }
  }

  public void clearTempData( final String dataId )
  {
    final File file = (File)m_mapDataId2File.get( dataId );
    if( file != null )
    {
      final boolean b = file.delete();

      if( !b )
        m_logger.warning( "Could not delete file " + file.toString() + " associated to dataId " + dataId );
    }
    else
      m_logger.warning( "Unknown dataId: " + dataId );
  }

  /**
   * @see org.kalypso.services.sensor.IObservationService#writeData(ObservationBean, DataHandler)
   */
  public void writeData( final ObservationBean obean, final DataHandler odb ) throws RemoteException
  {
    try
    {
      final IRepositoryItem item = itemFromBean( obean );

      final IObservation obs = (IObservation)item.getAdapter( IObservation.class );

      if( obs == null )
      {
        final RemoteException e = new RemoteException( "No observation for " + obean.getId() );
        m_logger.throwing( getClass().getName(), "writeData", e );
        throw e;
      }

      final IObservation zml = ZmlFactory.parseXML( new InputSource( odb.getInputStream() ), obs.getIdentifier(), null );

      obs.setValues( zml.getValues( null ) );
    }
    catch( final Exception e ) // generic exception caught for simplicity
    {
      m_logger.throwing( getClass().getName(), "writeData", e );
      throw new RemoteException( "", e );
    }
  }

  private IRepositoryItem itemFromBean( final ItemBean obean ) throws RemoteException, RepositoryException
  {
    if( obean == null )
    {
      final NullPointerException e = new NullPointerException( "ItemBean must not be null" );
      m_logger.throwing( getClass().getName(), "itemFromBean", e );
      throw e;
    }

    // maybe bean already in map?
    if( m_mapBeanId2Item.containsKey( obean.getId() ) )
      return (IRepositoryItem)m_mapBeanId2Item.get( obean.getId() );

    // try with repository id
    final String repId = RepositoryUtils.getRepositoryId( obean.getId() );
    if( m_mapRepId2Rep.containsKey( repId ) )
    {
      final IRepository rep = (IRepository)m_mapRepId2Rep.get( repId );

      final IRepositoryItem item = rep.findItem( obean.getId() );

      if( item == null )
        throw new RepositoryException( "Item does not exist or could not be found: " + obean.getId() );

      return item;
    }

    // last chance: go through repositories and use findItem()
    for( final Iterator it = m_repositories.iterator(); it.hasNext(); )
    {
      final IRepository rep = (IRepository)it.next();

      final IRepositoryItem item = rep.findItem( obean.getId() );

      if( item != null )
        return item;
    }

    final RemoteException e = new RemoteException( "Unknonwn Repository or item. Repository: " + repId + ", Item: "
        + obean.getId() );
    m_logger.throwing( getClass().getName(), "itemFromBean", e );
    throw e;
  }

  /**
   * @see org.kalypso.repository.service.IRepositoryService#hasChildren(org.kalypso.repository.service.ItemBean)
   */
  public boolean hasChildren( final ItemBean parent ) throws RemoteException
  {
    // dealing with ROOT?
    if( parent == null )
      return m_repositories.size() > 0;

    try
    {
      final IRepositoryItem item = itemFromBean( parent );

      return item.hasChildren();
    }
    catch( final RepositoryException e )
    {
      m_logger.throwing( getClass().getName(), "hasChildren", e );
      throw new RemoteException( "", e );
    }
  }

  /**
   * @see org.kalypso.repository.service.IRepositoryService#getChildren(org.kalypso.repository.service.ItemBean)
   */
  public ItemBean[] getChildren( final ItemBean pbean ) throws RemoteException
  {
    // dealing with ROOT?
    if( pbean == null )
    {
      if( m_repositoryBeans == null )
      {
        m_repositoryBeans = new ItemBean[m_repositories.size()];

        for( int i = 0; i < m_repositoryBeans.length; i++ )
        {
          final IRepository rep = (IRepository)m_repositories.get( i );

          m_repositoryBeans[i] = new RepositoryBean( rep.getIdentifier(), rep.getName() );
          m_mapBeanId2Item.put( m_repositoryBeans[i].getId(), rep );
        }
      }

      return m_repositoryBeans;
    }

    IRepositoryItem item = null;

    try
    {
      item = itemFromBean( pbean );

      // already in cache?
      if( m_mapItem2Bean.containsKey( item ) )
        return (ItemBean[])m_mapItem2Bean.get( item );

      final IRepositoryItem[] children = item.getChildren();

      final ItemBean[] beans = new ItemBean[children.length];
      for( int i = 0; i < beans.length; i++ )
      {
        beans[i] = new ItemBean( children[i].getIdentifier(), children[i].getName() );

        // store it for future referencing
        m_mapBeanId2Item.put( beans[i].getId(), children[i] );
      }

      // cache it for next request
      m_mapItem2Bean.put( item, beans );

      return beans;
    }
    catch( final RepositoryException e )
    {
      m_logger.throwing( getClass().getName(), "getChildren", e );
      throw new RemoteException( "", e );
    }
  }

  /**
   * @see org.kalypso.services.sensor.IObservationService#adaptItem(org.kalypso.repository.service.ItemBean)
   */
  public ObservationBean adaptItem( final ItemBean ib ) throws RemoteException
  {
    final IRepositoryItem item;
    try
    {
      item = itemFromBean( ib );
    }
    catch( RepositoryException e )
    {
      m_logger.throwing( getClass().getName(), "adaptItem", e );
      throw new RemoteException( "", e );
    }

    if( item == null )
      return null;

    final IObservation obs = (IObservation)item.getAdapter( IObservation.class );

    if( obs != null )
    {
      final MetadataList md = updateObservation( obs, ib.getId() );

      return new ObservationBean( ib.getId(), obs.getName(), md );
    }

    return null;
  }

  private MetadataList updateObservation( final IObservation obs, final String id )
  {
    // always update the observation metadata with the ocs-id
    final MetadataList md = obs.getMetadataList();
    md.setProperty( ZmlURLConstants.MD_OCS_ID, ZmlURL.addServerSideId( id ) );

    // look if there is a manipulator and let it update the observation
    final String repId = RepositoryUtils.getRepositoryId( id );
    final IObservationManipulator oman = (IObservationManipulator)m_mapRepId2Manip.get( repId );
    if( oman != null )
    {
      try
      {
        oman.manipulate( obs, id );
      }
      catch( final SensorException e )
      {
        m_logger.throwing( getClass().getName(), "updateMetadata", e );
        m_logger.info( "Could not manipulate observation with id: " + id + " due to previous errors" );
      }
    }

    return md;
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
    return "Kalypso Zeitreihendienst";
  }

  /**
   * @see org.kalypso.repository.service.IRepositoryService#reload()
   */
  public void reload() throws RemoteException
  {
    init();
  }

  /**
   * @see org.kalypso.repository.service.IRepositoryService#findItem(java.lang.String)
   */
  public ItemBean findItem( final String id ) throws RemoteException
  {
    for( Iterator it = m_repositories.iterator(); it.hasNext(); )
    {
      final IRepository rep = (IRepository)it.next();

      final IRepositoryItem item;

      // first check the repository itself, then look into it
      if( rep.getIdentifier().equals( id ) )
        item = rep;
      else
      {
        try
        {
          item = rep.findItem( id );
        }
        catch( RepositoryException e )
        {
          m_logger.throwing( getClass().getName(), "findItem", e );
          throw new RemoteException( "findItem()", e );
        }
      }

      if( item == null )
        continue;

      final ItemBean bean = new ItemBean( item.getIdentifier(), item.getName() );

      // store it for future referencing
      m_mapBeanId2Item.put( bean.getId(), item );

      return bean;
    }

    m_logger.warning( "Item not found: " + id );

    return null;
  }
}