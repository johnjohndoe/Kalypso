package org.kalypso.ui;

import java.io.IOException;
import java.io.InputStream;
import java.net.Authenticator;
import java.net.PasswordAuthentication;
import java.net.URL;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.MissingResourceException;
import java.util.Properties;
import java.util.ResourceBundle;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.UIManager;
import javax.xml.rpc.ServiceException;

import org.deegree_impl.extension.ITypeRegistry;
import org.deegree_impl.extension.TypeRegistrySingleton;
import org.deegree_impl.model.cs.ConvenienceCSFactoryFull;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.kalypso.loader.DefaultLoaderFactory;
import org.kalypso.loader.ILoaderFactory;
import org.kalypso.ogc.gml.table.celleditors.DefaultCellEditorFactory;
import org.kalypso.ogc.gml.table.celleditors.ICellEditorFactory;
import org.kalypso.ogc.sensor.deegree.ObservationLinkHandler;
import org.kalypso.ogc.sensor.view.ObservationCache;
import org.kalypso.repository.DefaultRepositoryContainer;
import org.kalypso.repository.RepositorySpecification;
import org.kalypso.services.IServicesConstants;
import org.kalypso.services.ProxyFactory;
import org.kalypso.services.ocs.OcsURLStreamHandler;
import org.kalypso.services.proxy.IObservationService;
import org.kalypso.ui.preferences.IKalypsoPreferences;
import org.kalypso.util.pool.ResourcePool;
import org.opengis.cs.CS_CoordinateSystem;
import org.osgi.framework.BundleContext;
import org.osgi.service.url.URLConstants;
import org.osgi.service.url.URLStreamHandlerService;

/**
 * The main plugin class to be used in the desktop.
 */
public class KalypsoGisPlugin extends AbstractUIPlugin
{
  private static final String BUNDLE_NAME = KalypsoGisPlugin.class.getPackage().getName()
      + ".resources.KalypsoGisPluginResources"; //$NON-NLS-N$

  /** location of the pool properties file */
  private static final String POOL_PROPERTIES = "resources/pools.properties"; //$NON-NLS-N$

  /** location of the observation properties file */
  private static final String OBSERVATION_REPOSITORIES_PROPERTIES = "resources/repositories.properties"; //$NON-NLS-N$

  private static KalypsoGisPlugin THE_PLUGIN = null;

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

  /** factory for webservice proxy for the kalypso client */
  private ProxyFactory m_proxyFactory;

  /** configuration of the client */
  private final Properties m_mainConf = new Properties();

  /**
   * The constructor. Manages the configuration of the kalypso client.
   */
  public KalypsoGisPlugin()
  {
    THE_PLUGIN = this;

    configureLogger();

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
    catch( final Exception e1 )
    {
      e1.printStackTrace();
    }

    registerTypeHandler();
  }

  /**
   * Loads the client configuration
   * 
   * @throws IllegalStateException
   *           if no configuration could be loaded
   */
  private void configure() throws IllegalStateException
  {
    // put system properties
    m_mainConf.putAll( System.getProperties() );

    // try to laod conf file
    final String[] locs = getPluginPreferences().getString( IKalypsoPreferences.CLIENT_CONF_URLS )
        .split( ";" );

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
        e.printStackTrace();

        String msg = "Konnte Konfigurationsdatei nicht laden: " + locs[i] + "\n";

        if( i == locs.length - 1 )
          msg += "Kalypso startet ohne Serverkonfiguration! Stelle Sie sicher dass mindestens ein Server zur Verfügung steht.\nOder prüfen Sie die Liste der Server in den Applikationseinstellungen.";
        else
          msg += "Es wird versucht, eine alternative Konfigurationsdatei zu laden.\nNächster Versuch:"
              + locs[i + 1];

        MessageDialog.openWarning( getWorkbench().getDisplay().getActiveShell(),
            "Konfiguration für Kalypso", msg );
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
  }

  /**
   * Loads the pool configuration
   */
  private void configurePool() throws IOException
  {
    myPoolProperties.load( this.getClass().getResourceAsStream( POOL_PROPERTIES ) );
  }

  /**
   * Sets service proxy factory specific properties and creates the proxy
   * factory object.
   */
  private void configureServiceProxyFactory()
  {
    // this is the base classname (actually just package name) of all the
    // kalypso service proxies
    m_mainConf.setProperty( ProxyFactory.KALYPSO_PROXY_BASE, "org.kalypso.services.proxy" );

    m_proxyFactory = new ProxyFactory( m_mainConf );
  }

  /**
   * Loads the properties related to the available repositories. For each of
   * these repositories, it creates a simple wrapper class that contains its
   * specification.
   */
  private void configureRepositorySpecifications() throws IOException
  {
    m_zmlRepositoriesProperties.load( getClass().getResourceAsStream(
        OBSERVATION_REPOSITORIES_PROPERTIES ) );

    final String[] available = m_zmlRepositoriesProperties.getProperty( "available" ).split( "," );

    m_repositoriesSpecification = new RepositorySpecification[available.length];

    for( int i = 0; i < available.length; i++ )
      m_repositoriesSpecification[i] = new RepositorySpecification( available[i],
          m_zmlRepositoriesProperties.getProperty( available[i] ), m_zmlRepositoriesProperties
              .getProperty( available[i] + "_FACTORY" ), m_zmlRepositoriesProperties
              .getProperty( available[i] + "_MAXCARD" ) );
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
   * Eclipse comes with its own StreamHandler proxy. So we just need to say which
   * Handler to use for the protocol we can cover.
   * <p>
   * Following handlers are registered:
   * <ul>
   * <li>OcsURLStreamHandler for 'kalypso-ocs' protocol. Handles Observation WebService urls.</li>
   * <li>TODO: insert your own handlers here...</li>
   * </ul> 
   */
  private void configureURLStreamHandler( final BundleContext context )
  {
    // register the observation webservice url stream handler
    final Hashtable properties = new Hashtable(1);
    properties.put(URLConstants.URL_HANDLER_PROTOCOL, new String[] {IServicesConstants.URL_PROTOCOL_OBSERVATION_SERVICE});
    context.registerService(URLStreamHandlerService.class.getName(), new OcsURLStreamHandler(), properties);
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
    return (IObservationService)m_proxyFactory.getProxy( "Kalypso_ObservationService",
        "IObservationService" );
  }

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
        MessageDialog.openError( getWorkbench().getDisplay().getActiveShell(),
            "Interne Applikationsfehler", e.getLocalizedMessage() );
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
    
    try
    {
      configure();
      configureProxy();
      configurePool();
      configureRepositorySpecifications();
      configureServiceProxyFactory();
      configureURLStreamHandler( context );
    }
    catch( final IOException e )
    {
      e.printStackTrace();
    }
    
  }

  /**
   * This method is called when the plug-in is stopped
   */
  public void stop( final BundleContext context ) throws Exception
  {
    super.stop( context );
    
    ObservationCache.dispose();
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
    // m_plugin should be set in the constructor
    if( THE_PLUGIN == null )
      throw new NullPointerException( "Plugin Kalypso noch nicht instanziert!" );

    return THE_PLUGIN;
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
  
  public Properties getDefaultRepositoryProperties()
  {
    final Properties props = new Properties();
    
    // set all known properties for repository
    final String value = getPluginPreferences().getString( IKalypsoPreferences.NUMBER_OF_DAYS );
    props.setProperty( IKalypsoPreferences.NUMBER_OF_DAYS, value );
    
    return props;
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
   * Erzeugt jedesmal eine neue Factory. N?tig, weil die Factory die
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

  private void registerTypeHandler()
  {
    final ITypeRegistry registry = TypeRegistrySingleton.getTypeRegistry();

    
    
    try
    {
      // TODO: read TypeHandler from property-file
      registry.registerTypeHandler( new ObservationLinkHandler() );
    }
    catch( Exception e ) // generic exception caught for simplicity
    {
      e.printStackTrace();
      
      MessageDialog.openError( getWorkbench().getDisplay().getActiveShell(),
          "Interne Applikationsfehler", e.getLocalizedMessage() );
    }
  }

  public int getDefaultMapSelectionID()
  {
    return 0x1;
  }

  private void configureProxy()
  {
    System.setProperty( "proxySet", getPluginPreferences().getString(
        IKalypsoPreferences.HTTP_PROXY_USE ) );
    System.setProperty( "proxyHost", getPluginPreferences().getString(
        IKalypsoPreferences.HTTP_PROXY_HOST ) );
    System.setProperty( "proxyPort", getPluginPreferences().getString(
        IKalypsoPreferences.HTTP_PROXY_PORT ) );

    Authenticator.setDefault( new Authenticator()
    {
      /**
       * @see java.net.Authenticator#getPasswordAuthentication()
       */
      protected PasswordAuthentication getPasswordAuthentication()
      {
        return new PasswordAuthentication( getPluginPreferences().getString(
            IKalypsoPreferences.HTTP_PROXY_USER ), getPluginPreferences().getString(
            IKalypsoPreferences.HTTP_PROXY_PASS ).toCharArray() );
      }
    } );
  }

  public static Status createErrorStatus( final String message, final Throwable cause )
  {
    String msg = message;
    if( cause != null && cause.getLocalizedMessage() != null )
      msg += ":\n\r" + cause.getLocalizedMessage();
    
    return new Status( IStatus.ERROR, getId(), 0, msg, cause );
  }

  /**
   * Clears the pool
   */
  public void clearPool()
  {
    myPools.clear();
  }
}