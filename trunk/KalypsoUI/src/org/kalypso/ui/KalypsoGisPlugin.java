package org.kalypso.ui;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.HashMap;
import java.util.MissingResourceException;
import java.util.Properties;
import java.util.ResourceBundle;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.UIManager;
import javax.xml.bind.JAXBException;
import javax.xml.rpc.ServiceException;
import javax.xml.rpc.Stub;

import org.deegree.tools.IURLConnectionFactory;
import org.deegree_impl.extension.ITypeRegistry;
import org.deegree_impl.extension.TypeRegistryException;
import org.deegree_impl.extension.TypeRegistrySingleton;
import org.deegree_impl.model.cs.ConvenienceCSFactoryFull;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.kalypso.loader.DefaultLoaderFactory;
import org.kalypso.loader.ILoaderFactory;
import org.kalypso.ogc.gml.table.celleditors.DefaultCellEditorFactory;
import org.kalypso.ogc.gml.table.celleditors.ICellEditorFactory;
import org.kalypso.ogc.sensor.deegree.ObservationLinkHandler;
import org.kalypso.repository.DefaultRepositoryContainer;
import org.kalypso.services.ProxyFactory;
import org.kalypso.services.proxy.IObservationService;
import org.kalypso.services.proxy.Kalypso_ObservationService_Impl;
import org.kalypso.ui.repository.RepositorySpecification;
import org.kalypso.util.pool.ResourcePool;
import org.opengis.cs.CS_CoordinateSystem;
import org.osgi.framework.BundleContext;

/**
 * The main plugin class to be used in the desktop.
 */
public class KalypsoGisPlugin extends AbstractUIPlugin
{
  /** name of the property where the client conf files can be found */
  public static final String CLIENT_CONF_URLS = "kalypso.client.conf";

  private static final String BUNDLE_NAME = KalypsoGisPlugin.class.getPackage().getName()
      + ".resources.KalypsoGisPluginResources"; //$NON-NLS-N$

  /** location of the pool properties file */
  private static final String POOL_PROPERTIES = "resources/pools.properties"; //$NON-NLS-N$

  /** location of the observation properties file */
  private static final String OBSERVATION_REPOSITORIES_PROPERTIES = "resources/observation_repositories.properties"; //$NON-NLS-N$

  private static KalypsoGisPlugin m_plugin = null;

  private ResourceBundle m_resourceBundle = null;

  private CS_CoordinateSystem myCoordinateSystem = null;

  private final HashMap myPools = new HashMap();

  private final HashMap myLoaderFactories = new HashMap();

  private final Properties myPoolProperties = new Properties();

  private Properties m_ftpProperties;

  /**
   * Contains the list of available repositories that can be selected by the
   * user. For each available repository, the IRepositoryFactory is provided.
   */
  private final Properties m_zmlRepositoriesProperties = new Properties();

  /** Manages the list of repositories. */
  private DefaultRepositoryContainer m_tsRepositoryContainer = null;

  /** The list of specifications for each repositories. */
  private RepositorySpecification[] m_repositoriesSpecification = null;

  private final SelectionIdProvider mySelectionIdProvider = new SelectionIdProvider();

  private IURLConnectionFactory m_urlConnectionFactory;

  /** factory for webservice proxy for the kalypso client */
  private ProxyFactory m_proxyFactory;

  /** configuration of the client */
  private Properties m_mainConf;

  /**
   * The constructor. Manages the configuration of the kalypso client.
   */
  public KalypsoGisPlugin()
  {
    m_plugin = this;

    configureLogger();

    try
    {
      prepareConfigure();
      configure();
      configureProxy();
      configurePool();
      configureObservationRepositorySpecifications();
      configureServiceProxyFactory();
    }
    catch( final IOException e )
    {
      e.printStackTrace();
    }

    try
    {
      m_resourceBundle = ResourceBundle.getBundle( BUNDLE_NAME );
    }
    catch( final MissingResourceException ex )
    {
      m_resourceBundle = null;

      ex.printStackTrace();
    }

    try
    {
      // for AWT and Swing stuff used with SWT_AWT so that they look like OS
      // controls
      UIManager.setLookAndFeel( UIManager.getSystemLookAndFeelClassName() );
    }
    catch( Exception e1 )
    {
      e1.printStackTrace();
    }

    registerTypeHandler();
  }

  /**
   * Prepare configuration of the Kalypso Client: reads the system properties
   * and the localisation(s) of the kalypso client configuration file.
   */
  private void prepareConfigure()
  {
    // get system properties and use them as main configuration
    m_mainConf = new Properties( System.getProperties() );

    // set the localisation of the Configuration files
    // TODO get this information from some external place (like Eclipse prefs)
    m_mainConf.setProperty( CLIENT_CONF_URLS, "http://pc242:8080/KalypsoConf/kalypso-client.ini" );
  }

  /**
   * Loads the client configuration
   * 
   * @throws IllegalStateException if no configuration could be loaded
   */
  private void configure() throws IllegalStateException
  {
    final String[] locs = m_mainConf.getProperty( CLIENT_CONF_URLS ).split( ";" );

    final Properties conf = new Properties();

    for( int i = 0; i < locs.length; i++ )
    {
      InputStream stream = null;

      try
      {
        final URL url = new URL( locs[i] );

        stream = url.openStream();

        conf.load( stream );
        
        m_mainConf.putAll( conf );
        
        return;
      }
      catch( Exception e ) // generic exception used to simplify processing
      {
        // do nothing, try with next location
      }
      finally
      {
        try
        {
          if( stream != null )
            stream.close();
        }
        catch( IOException e1 )
        {
          e1.printStackTrace();
        }
      }
    }
    
    throw new IllegalStateException( "Could not load client configuration from server!" );
  }

  /**
   * Loads the pool configuration
   */
  private void configurePool() throws IOException
  {
    myPoolProperties.load( this.getClass().getResourceAsStream( POOL_PROPERTIES ) );
  }

  /**
   * Sets service proxy factory specific properties and creates the proxy factory object.
   */
  private void configureServiceProxyFactory()
  {
    // this is the base classname (actually just package name) of all the kalypso service proxies
    m_mainConf.setProperty( ProxyFactory.KALYPSO_PROXY_BASE, "org.kalypso.services.proxy" );

    m_proxyFactory = new ProxyFactory( m_mainConf );
  }

  /**
   * Loads the properties related to the available repositories. For each of these repositories,
   * it creates a simple wrapper class that contains its specification.
   */
  private void configureObservationRepositorySpecifications() throws IOException
  {
    m_zmlRepositoriesProperties.load( getClass().getResourceAsStream(
        OBSERVATION_REPOSITORIES_PROPERTIES ) );

    final String[] available = m_zmlRepositoriesProperties.getProperty( "available" ).split( "," );

    m_repositoriesSpecification = new RepositorySpecification[available.length];

    for( int i = 0; i < available.length; i++ )
      m_repositoriesSpecification[i] = new RepositorySpecification( available[i],
          m_zmlRepositoriesProperties.getProperty( available[i] ), m_zmlRepositoriesProperties
              .getProperty( available[i] + "_FACTORY" ) );
  }

  private void configureLogger()
  {
    final Logger logger = Logger.getLogger( "org.kalypso" ); //$NON-NLS-N$
    logger.setLevel( Level.INFO );

    final Handler[] handlers = logger.getHandlers();
    for( int i = 0; i < handlers.length; i++ )
    {
      final Handler handler = handlers[i];
      handler.setLevel( Level.FINER );
    }
  }

  /**
   * Liefert die Liste der Konfigurierte und Zugreifbare Zeitreihen Repositories
   */
  public RepositorySpecification[] getRepositoriesSpecifications()
  {
    return m_repositoriesSpecification;
  }

  /**
   * Liefert die Kalypso Service ProxyFactory
   */
  public ProxyFactory getServiceProxyFactory()
  {
    return m_proxyFactory;
  }
  
  /**
   * Convenience method that returns the observation service proxy
   * 
   * @throws ServiceException
   */
  public IObservationService getObservationServiceProxy() throws ServiceException
  {
    final Kalypso_ObservationService_Impl koi = new Kalypso_ObservationService_Impl();
    final IObservationService port = koi.getIObservationServicePort();
    final String strEndPoint = "http://pc242:8080/Kalypso_ObservationService/IObservationServicePort";
    ((Stub)port)._setProperty( javax.xml.rpc.Stub.ENDPOINT_ADDRESS_PROPERTY, strEndPoint );
    return port;
    //return (IObservationService)m_proxyFactory.getProxy( "Kalypso_ObservationService", "IObservationService" );
  }

  //  TODO public OutputLogger getOutputLogger()
  //  {
  //    if( m_outputLogger == null )
  //      m_outputLogger = new OutputLogger();
  //
  //    return m_outputLogger;
  //  }

  public ILoaderFactory getLoaderFactory( final Class valueClass )
  {
    ILoaderFactory loaderFactory = (ILoaderFactory)myLoaderFactories.get( valueClass );

    if( loaderFactory == null )
    {
      final String propFilename = (String)myPoolProperties.get( valueClass.getName() );

      final Properties props = new Properties();
      try
      {
        props.load( KalypsoGisPlugin.class.getResourceAsStream( propFilename ) );
      }
      catch( IOException e )
      {
        // TODO besser handeln
        e.printStackTrace();
      }

      loaderFactory = new DefaultLoaderFactory( props, this.getClass().getClassLoader() );

      myLoaderFactories.put( valueClass, loaderFactory );
    }

    return loaderFactory;
  }

  public ResourcePool getPool( final Class valueClass )
  {
    ResourcePool pool = (ResourcePool)myPools.get( valueClass );
    if( pool == null )
    {
      pool = new ResourcePool( getLoaderFactory( valueClass ) );
      myPools.put( valueClass, pool );
    }

    return pool;
  }

  /**
   * This method is called upon plug-in activation
   */
  public void start( final BundleContext context ) throws Exception
  {
    super.start( context );
  }

  /**
   * This method is called when the plug-in is stopped
   */
  public void stop( final BundleContext context ) throws Exception
  {
    super.stop( context );
  }

  public static String getId()
  {
    return getDefault().getBundle().getSymbolicName();
  }

  /**
   * Returns the shared instance.
   */
  public static KalypsoGisPlugin getDefault()
  {
    // TODO: ist es sicher, das hier bereits was instantiiert wurde?
    return m_plugin;
  }

  /**
   * Returns the string from the plugin's resource bundle, or 'key' if not
   * found.
   */
  public static String getResourceString( final String key )
  {
    final ResourceBundle bundle = KalypsoGisPlugin.getDefault().getResourceBundle();
    try
    {
      return ( bundle != null ) ? bundle.getString( key ) : key;
    }
    catch( MissingResourceException e )
    {
      e.printStackTrace();

      return key;
    }
  }

  /**
   * Returns the plugin's resource bundle,
   */
  public ResourceBundle getResourceBundle()
  {
    return m_resourceBundle;
  }

  public CS_CoordinateSystem getCoordinatesSystem()
  {
    if( myCoordinateSystem == null )
    {
      ConvenienceCSFactoryFull csFac = new ConvenienceCSFactoryFull();
      myCoordinateSystem = org.deegree_impl.model.cs.Adapters.getDefault().export(
          csFac.getCSByName( "EPSG:4326" ) );
      //csFac.getCSByName( "EPSG:31494" ) );
    }
    return myCoordinateSystem;
  }

  public DefaultRepositoryContainer getRepositoryContainer()
  {
    if( m_tsRepositoryContainer == null )
      m_tsRepositoryContainer = new DefaultRepositoryContainer();

    return m_tsRepositoryContainer;
  }

  private Properties getFeatureTypeCellEditorProperties()
  {
    if( m_ftpProperties == null )
    {
      m_ftpProperties = new Properties();

      try
      {
        m_ftpProperties.load( KalypsoGisPlugin.class
            .getResourceAsStream( "resources/featureTypeEditor.properties" ) );
      }
      catch( final IOException e )
      {
        e.printStackTrace();
      }
    }

    return m_ftpProperties;
  }

  /**
   * Erzeugt jedesmal eine neue Factory. Nötig, weil die Factory die
   * CellEditoren cached, diese aber mit schliessen der Tabelle disposed werden.
   */
  public ICellEditorFactory createFeatureTypeCellEditorFactory()
  {
    return new DefaultCellEditorFactory( getFeatureTypeCellEditorProperties(), this.getClass()
        .getClassLoader() );
  }

  public SelectionIdProvider getSelectionIdProvider()
  {
    return mySelectionIdProvider;
  }

  //  public CalcJobService getCalcService()
  //  {
  //    if( m_calcJobService == null )
  //    {
  //      try
  //      {
  //        m_calcJobService = (CalcJobService)SimpleServiceFactory.createService(
  // "CalcJob.Default", CalcJobService.class );
  //      }
  //      catch( FactoryException e )
  //      {
  //        e.printStackTrace();
  //      }
  //    }
  //
  //    return m_calcJobService;
  //  }

  private void registerTypeHandler()
  {
    final ITypeRegistry registry = TypeRegistrySingleton.getTypeRegistry();

    // TODO: error handling
    try
    {
      // TODO: read TypeHandler from property-file
      registry.registerTypeHandler( new ObservationLinkHandler() );
    }
    catch( final TypeRegistryException e )
    {
      e.printStackTrace();
    }
    catch( JAXBException e )
    {
      e.printStackTrace();
    }
  }

  private void configureProxy()
  {
  //    System.setProperty( "proxySet", "true" );
  //    System.setProperty( "proxyHost", "172.16.0.1" );
  //    System.setProperty( "proxyPort", "8080" );
  //    String pw = "belger:LaufMensch";
  //    String epw = "Basic " + ( new BASE64Encoder() ).encode( pw.getBytes() );
  //
  //    m_urlConnectionFactory = new URLConnectionFactory( "Proxy-Authorization",
  // epw );
  //    NetWorker.setURLConnectionFactory( m_urlConnectionFactory );
  }

  public IURLConnectionFactory getURLConnectionFactory()
  {
    return m_urlConnectionFactory;
  }

  public int getDefaultMapSelectionID()
  {
    return 0x1;
  }

  //  private class URLConnectionFactory implements IURLConnectionFactory
  //  {
  //    private final String m_property;
  //
  //    private final String m_value;
  //
  //    private URLConnectionFactory( String property, String value )
  //    {
  //      m_property = property;
  //      m_value = value;
  //    }
  //
  //    private URLConnectionFactory()
  //    {
  //      m_property = null;
  //      m_value = null;
  //    }
  //
  //    /**
  //     * @see
  // org.deegree.tools.IURLConnectionFactory#createURLConnection(java.net.URL)
  //     */
  //    public URLConnection createURLConnection( URL url ) throws IOException
  //    {
  //      if( m_property == null )
  //        return url.openConnection();
  //      URLConnection connection = url.openConnection();
  //      connection.setRequestProperty( m_property, m_value );
  //      return connection;
  //    }
  //  }
}