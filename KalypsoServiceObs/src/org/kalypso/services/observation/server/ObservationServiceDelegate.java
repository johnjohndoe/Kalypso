/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.services.observation.server;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.rmi.RemoteException;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Properties;
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.activation.DataHandler;
import javax.activation.FileDataSource;

import org.apache.commons.io.IOUtils;
import org.eclipse.osgi.framework.internal.core.FrameworkProperties;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.java.lang.reflect.ClassUtilities;
import org.kalypso.contribs.java.net.UrlResolverSingleton;
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
import org.kalypso.services.observation.sei.DataBean;
import org.kalypso.services.observation.sei.IObservationService;
import org.kalypso.services.observation.sei.ItemBean;
import org.kalypso.services.observation.sei.ObservationBean;
import org.kalypso.services.observation.sei.RepositoryBean;
import org.kalypso.zml.Observation;
import org.kalypso.zml.request.Request;
import org.xml.sax.InputSource;

/**
 * Kalypso Observation Service.
 * <p>
 * When a observation is delivered to the client, the IObservationManipulator mechanism is always used to possibly
 * manipulate the observation before it is delivered. ObservationManipulators are configured within the
 * IObservationService configuration file. All entries that begin with "MANIPULATOR_" are defining such manipulators.
 * The syntax of the configuration is as follows: MANIPULATOR_&lt;repository_id&gt;=&lt;manipulator_class_name&gt;.
 * 
 * @author Marc Schlienger
 * @author Gernot Belger
 * @author Holger Albert
 */
public class ObservationServiceDelegate implements IObservationService
{
  private final List<IRepository> m_repositories;

  private ItemBean[] m_repositoryBeans = null;

  /** Bean-ID(String) --> IRepositoryItem */
  private final Map<String, IRepositoryItem> m_mapBeanId2Item;

  /** IRepositoryItem --> ItemBean */
  private final Map<IRepositoryItem, ItemBean[]> m_mapItem2Bean;

  /** Repository-ID(String) --> IRepository */
  private final Map<String, IRepository> m_mapRepId2Rep;

  /** Repository-ID(String) --> IObservationManipulator */
  private final Map<String, IObservationManipulator> m_mapRepId2Manip;

  /** Data-ID(String) --> File */
  private final Map<String, File> m_mapDataId2File;

  private final File m_tmpDir;

  private final Logger m_logger;

  private boolean m_initialized = false;

  /**
   * Constructs the service by reading the configuration.
   */
  public ObservationServiceDelegate( ) throws RepositoryException
  {
    m_repositories = new Vector<IRepository>();
    m_mapBeanId2Item = new Hashtable<String, IRepositoryItem>( 512 );
    m_mapItem2Bean = new Hashtable<IRepositoryItem, ItemBean[]>( 512 );
    m_mapRepId2Rep = new Hashtable<String, IRepository>();
    m_mapRepId2Manip = new Hashtable<String, IObservationManipulator>();
    m_mapDataId2File = new Hashtable<String, File>( 128 );

    m_logger = Logger.getLogger( ObservationServiceDelegate.class.getName() );
    m_initialized = false;

    m_tmpDir = FileUtilities.createNewTempDir( "Observations" );
    m_tmpDir.deleteOnExit();

    /* HINT: The init method tries to access another servlet in the same container. */
    init();
  }

  @Override
  protected void finalize( ) throws Throwable
  {
    // System.out.println( "Finalize observation service delegate (" + this.toString() + ") ... " +
    // DateFormat.getDateTimeInstance().format( Calendar.getInstance().getTime() ) );

    /* Dispose everything. */
    dispose();
  }

  /**
   * This function disposes everything.
   */
  public void dispose( )
  {
    clearCache();

    // force delete, even if we called deleteOnExit()
    if( m_tmpDir.exists() )
      m_tmpDir.delete();
  }

  private void clearCache( )
  {
    m_mapBeanId2Item.clear();
    m_mapItem2Bean.clear();
    m_mapRepId2Rep.clear();
    m_mapRepId2Manip.clear();
    m_repositoryBeans = null;

    // dispose repositories
    for( final Object element : m_repositories )
      ((IRepository) element).dispose();
    m_repositories.clear();

    // clear temp files
    for( final Object element : m_mapDataId2File.values() )
      ((File) element).delete();
    m_mapDataId2File.clear();

    ZmlFilter.configureFor( null );
  }

  /**
   * Initialise the Service according to configuration.
   * 
   * @throws RemoteException
   */
  protected final synchronized void init( ) throws RepositoryException
  {
    if( m_initialized )
      return;

    m_initialized = true;

    clearCache();

    final Properties props = new Properties();

    try
    {
      final String configurationLocation = FrameworkProperties.getProperty( "kalypso.hwv.observation.service.configuration.location" );
      final URL confLocation = new URL( configurationLocation );
      final URL confUrl = UrlResolverSingleton.resolveUrl( confLocation, "repconf_server.xml" );

      // this call also closes the stream
      final List<RepositoryFactoryConfig> facConfs = RepositoryConfigUtils.loadConfig( confUrl );

      // load the service properties
      final URL urlProps = UrlResolverSingleton.resolveUrl( confLocation, "service.properties" );

      InputStream ins = null;
      try
      {
        ins = urlProps.openStream();
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

      /* Configure logging according to configuration */
      try
      {
        final String logLevelString = props.getProperty( "LOG_LEVEL", Level.INFO.getName() );
        final Level logLevel = Level.parse( logLevelString );
        Logger.getLogger( "" ).setLevel( logLevel );
      }
      catch( final Throwable t )
      {
        // Catch everything, changing the log level should not prohibit this service to run
        t.printStackTrace();
      }

      /* Load Repositories */
      for( final Iterator< ? > it = facConfs.iterator(); it.hasNext(); )
      {
        final RepositoryFactoryConfig item = (RepositoryFactoryConfig) it.next();
        final IRepositoryFactory fact = item.getFactory();

        try
        {
          final IRepository rep = fact.createRepository();
          m_repositories.add( rep );

          m_mapRepId2Rep.put( rep.getIdentifier(), rep );

          // look into properties if an IObservationManipulator should be
          // configured for the current repository
          final String pManip = "MANIPULATOR_" + rep.getIdentifier();
          final String cnManip = props.getProperty( pManip );
          if( cnManip != null )
          {
            final IObservationManipulator man = (IObservationManipulator) ClassUtilities.newInstance( cnManip, IObservationManipulator.class, getClass().getClassLoader() );
            m_mapRepId2Manip.put( rep.getIdentifier(), man );
          }
        }
        catch( final Exception e )
        {
          m_logger.warning( "Could not create Repository " + fact.getRepositoryName() + " with configuration " + fact.getConfiguration() + ". Reason is:\n" + e.getLocalizedMessage() );
          e.printStackTrace();
        }
      }

      // tricky: set the list of repositories to the ZmlFilter so that
      // it can directly fetch the observations without using the default
      // URL resolving stuff
      ZmlFilter.configureFor( m_repositories );
    }
    catch( final Exception e ) // generic exception caught for simplicity
    {
      m_logger.throwing( getClass().getName(), "init", e );

      throw new RepositoryException( "Exception in KalypsoObservationService.init()", e );
    }
  }

  public DataBean readData( final String href ) throws SensorException
  {
    try
    {
      init();
    }
    catch( RepositoryException e1 )
    {
      throw new SensorException( e1 );
    }

    final String hereHref = ZmlURL.removeServerSideId( href );
    final String obsId = ZmlURL.getIdentifierPart( hereHref );
    final ObservationBean obean = new ObservationBean( obsId );

    // request part specified?
    IRequest request = null;
    Request requestType = null;
    try
    {
      requestType = RequestFactory.parseRequest( hereHref );
      if( requestType != null )
      {
        request = ObservationRequest.createWith( requestType );

        m_logger.info( "Reading data for observation: " + obean.getId() + " Request: " + request );
      }
      else
        m_logger.info( "Reading data for observation: " + obean.getId() );
    }
    catch( final SensorException e )
    {
      m_logger.warning( "Invalid Href: " + href );
      m_logger.throwing( getClass().getName(), "readData", e );

      // this is a fatal error (software programming error on the client-side)
      // so break processing now!
      throw e;
    }

    // fetch observation from repository
    IObservation obs = null;
    try
    {
      final IRepositoryItem item = itemFromBean( obean );

      obs = (IObservation) item.getAdapter( IObservation.class );
    }
    catch( final Exception e )
    {
      m_logger.info( "Could not find an observation for " + obean.getId() + ". Reason is:\n" + e.getLocalizedMessage() );

      // this is not a fatal error, repository might be temporarely unavailable
    }

    if( obs == null )
    {
      // obs could not be created, use the request now
      m_logger.info( "Creating request-based observation for " + obean.getId() );
      obs = RequestFactory.createDefaultObservation( requestType );
    }

    // and eventually manipulate the observation
    updateObservation( obs, obean.getId() );

    FileOutputStream fos = null;
    try
    {
      // tricky: maybe make a filtered observation out of this one
      obs = FilterFactory.createFilterFrom( hereHref, obs, null );

      final Observation obsType = ZmlFactory.createXML( obs, request, null );

      // name of the temp file must be valid against OS-rules for naming files
      // so remove any special characters
      final String tempFileName = FileUtilities.validateName( "___" + obs.getName(), "-" );

      // create temp file
      m_tmpDir.mkdirs(); // additionally create the parent dir if not already exists
      final File f = File.createTempFile( tempFileName, ".zml", m_tmpDir );

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
      throw new SensorException( e.getLocalizedMessage(), e );
    }
    finally
    {
      if( fos != null )
        try
        {
          fos.close();
        }
        catch( final IOException e )
        {
          m_logger.severe( e.getLocalizedMessage() );
          throw new SensorException( "Error closing the output stream", e );
        }
    }
  }

  public void clearTempData( final String dataId )
  {
    final File file = m_mapDataId2File.get( dataId );
    if( file != null )
    {
      final boolean b = file.delete();

      if( !b )
        m_logger.warning( "Could not delete file " + file.toString() + " associated to dataId " + dataId );
    }
    else
      m_logger.warning( "Unknown dataId: " + dataId );
  }

  public void writeData( final ObservationBean obean, final DataHandler odb ) throws SensorException
  {
    try
    {
      init();

      final IRepositoryItem item = itemFromBean( obean );

      final IObservation obs = (IObservation) item.getAdapter( IObservation.class );

      if( obs == null )
      {
        final RemoteException e = new RemoteException( "No observation for " + obean.getId() );
        m_logger.throwing( getClass().getName(), "writeData", e );
        throw e;
      }

      final IObservation zml = ZmlFactory.parseXML( new InputSource( odb.getInputStream() ), obs.getIdentifier(), null );

      synchronized( obs )
      {
        obs.setValues( zml.getValues( null ) );
      }
    }
    catch( final Exception e ) // generic exception caught for simplicity
    {
      m_logger.throwing( getClass().getName(), "writeData", e );
      throw new SensorException( e.getLocalizedMessage(), e );
    }
  }

  /**
   * @throws NoSuchElementException
   *           if item and/or repository not found
   */
  private IRepositoryItem itemFromBean( final ItemBean obean ) throws RepositoryException, NoSuchElementException
  {
    if( obean == null )
      throw new NullPointerException( "ItemBean must not be null" );

    /* Create the repository beans, if neccessary. */
    createRepositoryBeans();

    final String id = ZmlURL.removeServerSideId( obean.getId() );

    // maybe bean already in map?
    if( m_mapBeanId2Item.containsKey( id ) )
      return m_mapBeanId2Item.get( id );

    // try with repository id
    final String repId = RepositoryUtils.getRepositoryId( id );
    if( m_mapRepId2Rep.containsKey( repId ) )
    {
      final IRepository rep = m_mapRepId2Rep.get( repId );

      final IRepositoryItem item = rep.findItem( id );

      if( item == null )
        throw new NoSuchElementException( "Item does not exist or could not be found: " + id );

      return item;
    }

    // last chance: go through repositories and use findItem()
    for( final Object element : m_repositories )
    {
      final IRepository rep = (IRepository) element;

      final IRepositoryItem item = rep.findItem( id );

      if( item != null )
        return item;
    }

    throw new NoSuchElementException( "Unknown Repository or item. Repository: " + repId + ", Item: " + id );
  }

  /**
   * @see org.kalypso.repository.service.IRepositoryService#hasChildren(org.kalypso.repository.service.ItemBean)
   */
  public boolean hasChildren( final ItemBean parent ) throws RepositoryException
  {
    init();

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
      throw e;
    }
  }

  /**
   * @see org.kalypso.repository.service.IRepositoryService#getChildren(org.kalypso.repository.service.ItemBean)
   */
  public ItemBean[] getChildren( final ItemBean pbean ) throws RepositoryException
  {
    init();

    // dealing with ROOT?
    if( pbean == null )
    {
      createRepositoryBeans();
      return m_repositoryBeans;
    }

    IRepositoryItem item = null;

    try
    {
      item = itemFromBean( pbean );

      // already in cache?
      if( m_mapItem2Bean.containsKey( item ) )
        return m_mapItem2Bean.get( item );

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
      throw e;
    }
  }

  /**
   * This function creates the repository beans, if neccessary.
   */
  private void createRepositoryBeans( )
  {
    if( m_repositoryBeans == null )
    {
      m_repositoryBeans = new ItemBean[m_repositories.size()];

      for( int i = 0; i < m_repositoryBeans.length; i++ )
      {
        final IRepository rep = m_repositories.get( i );

        m_repositoryBeans[i] = new RepositoryBean( rep.getIdentifier(), rep.getName() );
        m_mapBeanId2Item.put( m_repositoryBeans[i].getId(), rep );
      }
    }
  }

  /**
   * @see org.kalypso.services.sensor.IObservationService#adaptItem(org.kalypso.repository.service.ItemBean)
   */
  public ObservationBean adaptItem( final ItemBean ib ) throws SensorException
  {
    try
    {
      init();

      final IRepositoryItem item = itemFromBean( ib );
      if( item == null )
        return null;

      final IObservation obs = (IObservation) item.getAdapter( IObservation.class );

      if( obs == null )
        return null;

      final MetadataList md = updateObservation( obs, ib.getId() );

      return new ObservationBean( ib.getId(), obs.getName(), md );
    }
    catch( final RepositoryException e )
    {
      m_logger.throwing( getClass().getName(), "adaptItem", e );
      throw new SensorException( e.getLocalizedMessage(), e );
    }
  }

  private MetadataList updateObservation( final IObservation obs, final String id )
  {
    // always update the observation metadata with the ocs-id
    final MetadataList md = obs.getMetadataList();
    md.setProperty( ZmlURLConstants.MD_OCS_ID, ZmlURL.addServerSideId( id ) );

    // look if there is a manipulator and let it update the observation
    final String repId = RepositoryUtils.getRepositoryId( id );
    final IObservationManipulator oman = m_mapRepId2Manip.get( repId );
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
  public int getServiceVersion( )
  {
    return 0;
  }

  /**
   * @see org.kalypso.repository.service.IRepositoryService#reload()
   */
  public void reload( ) throws RepositoryException
  {
    m_initialized = false;

    init();
  }

  /**
   * @see org.kalypso.repository.service.IRepositoryService#findItem(java.lang.String)
   */
  public ItemBean findItem( final String id ) throws RepositoryException
  {
    init();

    for( final Object element : m_repositories )
    {
      final IRepository rep = (IRepository) element;

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
        catch( final RepositoryException e )
        {
          m_logger.throwing( getClass().getName(), "findItem", e );
          throw e;
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