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
import java.util.Vector;
import java.util.logging.FileHandler;
import java.util.logging.Logger;

import javax.activation.DataHandler;
import javax.activation.DataSource;
import javax.activation.FileDataSource;

import org.kalypso.java.io.FileUtilities;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.beans.DateRangeBean;
import org.kalypso.ogc.sensor.beans.ObservationBean;
import org.kalypso.ogc.sensor.filter.filters.ZmlFilter;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.repository.IRepository;
import org.kalypso.repository.IRepositoryItem;
import org.kalypso.repository.RepositoryException;
import org.kalypso.repository.beans.ItemBean;
import org.kalypso.repository.conf.RepositoryConfigUtils;
import org.kalypso.repository.conf.RepositoryFactoryConfig;
import org.kalypso.repository.factory.IRepositoryFactory;
import org.kalypso.services.common.ServiceConfig;
import org.kalypso.services.sensor.IObservationService;
import org.kalypso.util.runtime.args.DateRangeArgument;
import org.kalypso.zml.ObservationType;
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

  private final Map m_mapBean2Item;

  private final Map m_mapItem2Beans;

  private final Map m_mapId2Rep;

  private final File m_tmpDir;

  private final Logger m_logger;

  /**
   * Constructs the service by reading the configuration.
   * 
   * @throws RemoteException
   */
  public KalypsoObservationService( ) throws RemoteException
  {
    m_repositories = new Vector();
    m_mapBean2Item = new Hashtable( 512 );
    m_mapItem2Beans = new Hashtable( 512 );
    m_mapId2Rep = new Hashtable();

    m_logger = Logger.getLogger( KalypsoObservationService.class.getName() );

    try
    {
      m_logger.addHandler( new FileHandler( ServiceConfig.getTempDir()
          + "/IObservation%g.log", 10000000, 1, true ) );
    }
    catch( Exception e ) // generic Exception caught for simplicity
    {
      e.printStackTrace();
      throw new RemoteException( "Exception in constructor von: "
          + getClass().getName(), e );
    }

    m_tmpDir = FileUtilities.createNewTempDir( "Observations", ServiceConfig
        .getTempDir() );
    m_tmpDir.deleteOnExit();

    init();
  }

  private final void clearRepositories()
  {
    for( final Iterator it = m_repositories.iterator(); it.hasNext(); )
      ((IRepository) it.next()).dispose();
    
    m_repositories.clear();
  }
  
  /**
   * Initialize the Service according to configuration.
   * 
   * @throws RemoteException
   */
  private final void init( ) throws RemoteException
  {
    m_mapBean2Item.clear();
    m_mapItem2Beans.clear();
    m_mapId2Rep.clear();
    m_repositoryBeans = null;
    clearRepositories();

    try
    {
      final File conf = new File( ServiceConfig.getConfDir(),
          "IObservationService/repconf_server.xml" );

      final InputStream stream = new FileInputStream( conf );

      // this call also closes the stream
      final List facConfs = RepositoryConfigUtils.loadConfig( stream );

      for( final Iterator it = facConfs.iterator(); it.hasNext(); )
      {
        final RepositoryFactoryConfig item = (RepositoryFactoryConfig) it
            .next();
        final IRepositoryFactory fact = item.createFactory( getClass()
            .getClassLoader() );

        final IRepository rep = fact.createRepository();
        m_repositories.add( rep );

        m_mapId2Rep.put( rep.getIdentifier(), rep );
      }

      // tricky: set the list of repositories to the ZmlFilter so that
      // it can directly fetch the observations without using the default
      // URL resolving stuff
      ZmlFilter.configureFor( m_repositories );
    }
    catch( Exception e ) // generic exception caught for simplicity
    {
      m_logger.throwing( getClass().getName(), "init", e );

      throw new RemoteException(
          "Exception in KalypsoObservationService.init()", e );
    }
  }

  /**
   * Performs some clean up.
   * 
   * @see java.lang.Object#finalize()
   */
  protected void finalize( ) throws Throwable
  {
    clearRepositories();
    m_repositoryBeans = null;
    m_mapBean2Item.clear();
    m_mapItem2Beans.clear();
    m_mapId2Rep.clear();

    ZmlFilter.configureFor( null );

    // force delete, even if we called deleteOnExit()
    m_tmpDir.delete();
  }

  /**
   * @see org.kalypso.services.sensor.IObservationService#readData(org.kalypso.ogc.sensor.beans.ObservationBean,
   *      org.kalypso.ogc.sensor.beans.DateRangeBean)
   */
  public DataHandler readData( final ObservationBean obean,
      final DateRangeBean drb ) throws RemoteException
  {
    FileOutputStream fos = null;

    try
    {
      final IRepositoryItem item = itemFromBean( obean );

      final IObservation obs = (IObservation) item
          .getAdapter( IObservation.class );

      if( obs == null )
      {
        final RemoteException e = new RemoteException( "No observation for "
            + obean.getId() );
        m_logger.throwing( getClass().getName(), "readData", e );
        throw e;
      }

      DateRangeArgument args = null;
      if( drb != null )
        args = new DateRangeArgument( drb.getFrom(), drb.getTo() );

      m_logger.info( "Reading data for observation: " + obs.getName()
          + " Arguments: " + args );

      final ObservationType obsType = ZmlFactory.createXML( obs, args );

      final File f = File.createTempFile( "___" + obs.getName(), ".zml",
          m_tmpDir );

      // we say delete on exit even if we allow the client to delete the file
      // explicitely in the clearTempData() service call. This allows us to
      // clear
      // temp files on shutdown in the case the client forgets it.
      f.deleteOnExit();

      // will be closed in finally block
      fos = new FileOutputStream( f );
      ZmlFactory.getMarshaller().marshal( obsType, fos );

      final DataHandler data = new DataHandler( new FileDataSource( f ) );
      return data;
    }
    catch( Exception e ) // generic exception used for simplicity
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

  /**
   * @see org.kalypso.services.sensor.IObservationService#clearTempData(javax.activation.DataHandler)
   */
  public void clearTempData( final DataHandler data )
  {
    final DataSource ds = data.getDataSource();
    if( ds instanceof FileDataSource )
    {
      final File file = ((FileDataSource) ds).getFile();
      file.delete();
    }
    else
      m_logger.warning( "Could not delete data associated to: " + data );
  }

  /**
   * @see org.kalypso.services.sensor.IObservationService#writeData(ObservationBean, DataHandler)
   */
  public void writeData( final ObservationBean obean, final DataHandler odb )
      throws RemoteException
  {
    try
    {
      final IRepositoryItem item = itemFromBean( obean );

      final IObservation obs = (IObservation) item
          .getAdapter( IObservation.class );

      if( obs == null )
      {
        final RemoteException e = new RemoteException( "No observation for "
            + obean.getId() );
        m_logger.throwing( getClass().getName(), "writeData", e );
        throw e;
      }

      final IObservation zml = ZmlFactory.parseXML( new InputSource( odb
          .getInputStream() ), obs.getIdentifier(), null );

      obs.setValues( zml.getValues( null ) );
    }
    catch( Exception e ) // generic exception caught for simplicity
    {
      m_logger.throwing( getClass().getName(), "writeData", e );
      throw new RemoteException( "", e );
    }
  }

  /**
   * Helper
   * 
   * @param obean
   * @return item
   * 
   * @throws RemoteException
   * @throws RepositoryException
   */
  private IRepositoryItem itemFromBean( final ItemBean obean )
      throws RemoteException, RepositoryException
  {
    if( obean == null )
    {
      final NullPointerException e = new NullPointerException(
          "ItemBean must not be null" );
      m_logger.throwing( getClass().getName(), "itemFromBean", e );
      throw e;
    }

    // maybe bean already in map?
    if( m_mapBean2Item.containsKey( obean.getId() ) )
      return (IRepositoryItem) m_mapBean2Item.get( obean.getId() );

    // try with repository id
    if( m_mapId2Rep.containsKey( obean.getRepId() ) )
    {
      final IRepository rep = (IRepository) m_mapId2Rep.get( obean.getRepId() );

      final IRepositoryItem item = rep.findItem( obean.getId() );

      if( item == null )
        throw new RepositoryException(
            "Item does not exist or could not be found: " + obean.getId() );

      return item;
    }

    // last chance: go through repositories and use findItem()
    for( final Iterator it = m_repositories.iterator(); it.hasNext(); )
    {
      final IRepository rep = (IRepository) it.next();

      final IRepositoryItem item = rep.findItem( obean.getId() );

      if( item != null )
        return item;
    }

    final RemoteException e = new RemoteException(
        "Unknonwn Repository or item. Repository: " + obean.getRepId()
            + ", Item: " + obean.getId() );
    m_logger.throwing( getClass().getName(), "itemFromBean", e );
    throw e;
  }

  /**
   * @see org.kalypso.repository.service.IRepositoryService#hasChildren(org.kalypso.repository.beans.ItemBean)
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
    catch( RepositoryException e )
    {
      m_logger.throwing( getClass().getName(), "hasChildren", e );
      throw new RemoteException( "", e );
    }
  }

  /**
   * @see org.kalypso.repository.service.IRepositoryService#getChildren(org.kalypso.repository.beans.ItemBean)
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
          final IRepository rep = (IRepository) m_repositories.get( i );

          m_repositoryBeans[i] = new ItemBean( rep.getIdentifier(), rep
              .getName(), rep.getIdentifier() );
          m_mapBean2Item.put( m_repositoryBeans[i].getId(), rep );
        }
      }

      return m_repositoryBeans;
    }

    IRepositoryItem item = null;

    try
    {
      item = itemFromBean( pbean );

      // already in cache?
      if( m_mapItem2Beans.containsKey( item ) )
        return (ItemBean[]) m_mapItem2Beans.get( item );

      final IRepositoryItem[] children = item.getChildren();

      // TODO null pointer exception on children. sometimes
      final ItemBean[] beans = new ItemBean[children.length];

      for( int i = 0; i < beans.length; i++ )
      {
        beans[i] = new ItemBean( children[i].getIdentifier(), children[i]
            .getName(), item.getRepository().getIdentifier() );

        // store it for future referencing
        m_mapBean2Item.put( beans[i].getId(), children[i] );
      }

      // cache it for next request
      m_mapItem2Beans.put( item, beans );

      return beans;
    }
    catch( RepositoryException e )
    {
      m_logger.throwing( getClass().getName(), "getChildren", e );
      throw new RemoteException( "", e );
    }
  }

  /**
   * @see org.kalypso.services.sensor.IObservationService#adaptItem(org.kalypso.repository.beans.ItemBean)
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

    final IObservation obs = (IObservation) item
        .getAdapter( IObservation.class );

    if( obs != null )
    {
      return new ObservationBean( ib.getId(), obs.getName(), item
          .getRepository().getIdentifier(), obs.getMetadataList() );
    }

    return null;
  }

  /**
   * @see org.kalypso.services.sensor.IObservationService#getServiceVersion()
   */
  public int getServiceVersion( )
  {
    return 0;
  }

  /**
   * @see org.kalypso.services.sensor.IObservationService#getDescription()
   */
  public String getDescription( )
  {
    return "Kalypso Zeitreihendienst";
  }

  /**
   * @see org.kalypso.repository.service.IRepositoryService#reload()
   */
  public void reload( ) throws RemoteException
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
      final IRepository rep = (IRepository) it.next();

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

      ItemBean bean = new ItemBean( item.getIdentifier(), item.getName(), item
          .getRepository().getIdentifier() );

      // store it for future referencing
      m_mapBean2Item.put( bean.getId(), item );

      return bean;
    }

    m_logger.warning( "Item not found: " + id );

    return null;
  }
}