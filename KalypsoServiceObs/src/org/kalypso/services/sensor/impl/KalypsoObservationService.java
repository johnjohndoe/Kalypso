package org.kalypso.services.sensor.impl;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.rmi.RemoteException;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import org.kalypso.java.io.FileUtilities;
import org.kalypso.java.lang.reflect.ClassUtilities.ClassUtilityException;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.beans.DateRangeBean;
import org.kalypso.ogc.sensor.beans.OCSDataBean;
import org.kalypso.ogc.sensor.beans.ObservationBean;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.ogc.sensor.zml.ZmlObservation;
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
import org.kalypso.util.runtime.args.DateRangeArgument;
import org.kalypso.zml.ObservationType;

/**
 * Kalypso Observation Service.
 * 
 * @author schlienger
 */
public class KalypsoObservationService implements IObservationService
{
  private List m_repositories = null;

  private ItemBean[] m_repositoryBeans = null;

  private final Map m_mapBean2Item;

  private final Map m_mapBean2File;

  private final Map m_mapItem2Beans;

  private final Map m_mapId2Rep;

  private final File m_tmpDir;

  private int m_lastId = 0;

  /**
   * Constructs the service by reading the configuration.
   * 
   * @throws RepositoryException
   * @throws ClassUtilityException
   * @throws FileNotFoundException
   */
  public KalypsoObservationService() throws RepositoryException, ClassUtilityException,
      FileNotFoundException
  {
    m_mapBean2Item = new Hashtable( 512 );
    m_mapBean2File = new Hashtable( 512 );
    m_mapItem2Beans = new Hashtable( 512 );
    m_mapId2Rep = new Hashtable();

    m_tmpDir = FileUtilities.createNewTempDir( "Observations", ServiceConfig.getTempDir() );

    init();
  }

  /**
   * Initialize the Service according to configuration.
   */
  private void init() throws FileNotFoundException, RepositoryException, ClassUtilityException
  {
    m_lastId = 0;

    m_mapBean2File.clear();
    m_mapBean2Item.clear();
    m_mapItem2Beans.clear();
    m_mapId2Rep.clear();

    final File conf = new File( ServiceConfig.getConfDir(),
        "IObservationService/repconf_server.xml" );

    final InputStream stream = new FileInputStream( conf );

    // this call also closes the stream
    final RepositoryConfig config = RepositoryConfigUtils.loadConfig( stream );

    final List items = config.getItems();
    m_repositories = new Vector( items.size() );

    for( final Iterator it = items.iterator(); it.hasNext(); )
    {
      final RepositoryConfigItem item = (RepositoryConfigItem)it.next();
      final IRepositoryFactory fact = item.createFactory();

      final IRepository rep = fact.createRepository();
      m_repositories.add( rep );

      m_mapId2Rep.put( rep.getIdentifier(), rep );
    }
  }

  /**
   * @see org.kalypso.services.sensor.IObservationService#readData(org.kalypso.ogc.sensor.beans.ObservationBean, org.kalypso.ogc.sensor.beans.DateRangeBean)
   */
  public OCSDataBean readData( final ObservationBean obean, final DateRangeBean drb ) throws RemoteException
  {
    FileOutputStream fos = null;

    try
    {
      final IRepositoryItem item = itemFromBean( obean );

      final IObservation obs = (IObservation)item.getAdapter( IObservation.class );
      
      if( obs == null )
        throw new RemoteException( "No observation for " + obean.getId() );

      final DateRangeArgument args = new DateRangeArgument( drb.getFrom(), drb.getTo() );
      final ObservationType obsType = ZmlFactory.createXML( obs, args );

      final File f = File.createTempFile( "___" + obs.getName(), ".zml", m_tmpDir );

      // will be closed in finally block
      fos = new FileOutputStream( f );
      ZmlFactory.getMarshaller().marshal( obsType, fos );

      final OCSDataBean oddb = new OCSDataBean( m_lastId++, obean.getId(), f.toURL().toExternalForm() );

      // DATABEAN --> ZML File
      m_mapBean2File.put( new Integer( oddb.getId() ), f );

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
   * @see org.kalypso.services.sensor.IObservationService#clearTempData(org.kalypso.ogc.sensor.beans.OCSDataBean)
   */
  public void clearTempData( final OCSDataBean bean )
  {
    final Integer id = new Integer( bean.getId() );

    // DATA-BEAN --> Zml-File
    if( m_mapBean2File.containsKey( id ) )
    {
      final File f = (File)m_mapBean2File.get( id );
      f.delete();

      m_mapBean2File.remove( id );
    }
  }

  /**
   * @see org.kalypso.services.sensor.IObservationService#writeData(org.kalypso.ogc.sensor.beans.ObservationBean,
   *      org.kalypso.ogc.sensor.beans.OCSDataBean)
   */
  public void writeData( final ObservationBean obean, final OCSDataBean odb )
      throws RemoteException
  {
    try
    {
      final IRepositoryItem item = itemFromBean( obean );

      final IObservation obs = (IObservation)item.getAdapter( IObservation.class );
      
      if( obs == null )
        throw new RemoteException( "No observation for " + obean.getId() );
      
      ZmlObservation zml = new ZmlObservation( new URL( odb.getLocation() ), odb.getObsId() );
      
      obs.setValues( zml.getValues( null ) );
    }
    catch( Exception e ) // generic exception caught for simplicity
    {
      throw new RemoteException( "", e );
    }

  }

  /**
   * Helper
   * 
   * @throws RemoteException
   * @throws RepositoryException
   */
  private IRepositoryItem itemFromBean( final ItemBean obean ) throws RemoteException,
      RepositoryException
  {
    if( obean == null )
      throw new NullPointerException( "ItemBean must not be null" );

    if( m_mapBean2Item.containsKey( obean.getId() ) )
      return (IRepositoryItem)m_mapBean2Item.get( obean.getId() );

    if( m_mapId2Rep.containsKey( obean.getRepId() ) )
    {
      final IRepository rep = (IRepository)m_mapId2Rep.get( obean.getRepId() );

      return rep.findItem( obean.getId() );
    }
    else
      throw new RemoteException( "Unknonwn Repository: " + obean.getRepId() );
  }

  /**
   * @see org.kalypso.services.repository.IRepositoryService#hasChildren(org.kalypso.repository.beans.ItemBean)
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
      e.printStackTrace();
      throw new RemoteException( "", e );
    }
  }

  /**
   * @see org.kalypso.services.repository.IRepositoryService#getChildren(org.kalypso.repository.beans.ItemBean)
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

          m_repositoryBeans[i] = new ItemBean( rep.getIdentifier(), rep.getName(), rep
              .getIdentifier() );
          m_mapBean2Item.put( m_repositoryBeans[i].getId(), rep );
        }
      }

      return m_repositoryBeans;
    }

    IRepositoryItem item = null;

    try
    {
      item = itemFromBean( pbean );
    }
    catch( RepositoryException e )
    {
      e.printStackTrace();
      throw new RemoteException( "", e );
    }

    // already in cache?
    if( m_mapItem2Beans.containsKey( item ) )
      return (ItemBean[])m_mapItem2Beans.get( item );

    final IRepositoryItem[] children = item.getChildren();
    final ItemBean[] beans = new ItemBean[children.length];

    for( int i = 0; i < beans.length; i++ )
    {
      final IObservation obs = (IObservation)children[i].getAdapter( IObservation.class );
      if( obs != null )
      {
        beans[i] = new ObservationBean( children[i].getIdentifier(), obs.getName(), item.getRepository()
            .getIdentifier(), obs.getMetadataList() );
      }
      else
      {
        beans[i] = new ItemBean( children[i].getIdentifier(), children[i].getName(), item.getRepository()
            .getIdentifier() );
      }

      // store it for future referencing
      m_mapBean2Item.put( beans[i].getId(), children[i] );
    }

    // cache it for next request
    m_mapItem2Beans.put( item, beans );

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
    return "Kalypso Zeitreihendienst";
  }

  /**
   * @see org.kalypso.services.repository.IRepositoryService#reload()
   */
  public void reload() throws RemoteException
  {
    m_repositoryBeans = null;

    for( Iterator it = m_repositories.iterator(); it.hasNext(); )
    {
      final IRepository rep = (IRepository)it.next();

      try
      {
        rep.reload();
      }
      catch( RepositoryException e )
      {
        e.printStackTrace();
        throw new RemoteException( "", e );
      }
    }

    try
    {
      init();
    }
    catch( Exception e ) // generic exception caught for simplicity
    {
      e.printStackTrace();
      throw new RemoteException( "", e );
    }
  }

  /**
   * @see org.kalypso.services.repository.IRepositoryService#findItem(java.lang.String)
   */
  public ItemBean findItem( String id ) throws RemoteException
  {
    for( Iterator it = m_repositories.iterator(); it.hasNext(); )
    {
      final IRepository rep= (IRepository)it.next();
      
      try
      {
        final IRepositoryItem item = rep.findItem( id );
        
        ItemBean bean = null;
        
        final IObservation obs = (IObservation)item.getAdapter( IObservation.class );
        if( obs != null )
        {
          bean = new ObservationBean( item.getIdentifier(), obs.getName(), item.getRepository()
              .getIdentifier(), obs.getMetadataList() );
        }
        else
        {
          bean = new ItemBean( item.getIdentifier(), item.getName(), item.getRepository()
              .getIdentifier() );
        }

        // store it for future referencing
        m_mapBean2Item.put( bean.getId(), item );
        
        return bean;
      }
      catch( RepositoryException e )
      {
        // ignored
      }
    }
    
    throw new RemoteException( "Item not found: " + id );
  }
}
