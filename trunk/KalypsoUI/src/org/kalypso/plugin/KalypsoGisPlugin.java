package org.kalypso.plugin;

import java.io.IOException;
import java.net.URL;
import java.net.URLConnection;
import java.util.HashMap;
import java.util.MissingResourceException;
import java.util.Properties;
import java.util.ResourceBundle;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.UIManager;
import javax.xml.bind.JAXBException;

import org.deegree.tools.IURLConnectionFactory;
import org.deegree_impl.extension.ITypeRegistry;
import org.deegree_impl.extension.TypeRegistryException;
import org.deegree_impl.extension.TypeRegistrySingleton;
import org.deegree_impl.model.cs.ConvenienceCSFactoryFull;
import org.deegree_impl.tools.NetWorker;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.kalypso.eclipse.jface.viewers.DefaultCellEditorFactory;
import org.kalypso.eclipse.jface.viewers.ICellEditorFactory;
import org.kalypso.loader.DefaultLoaderFactory;
import org.kalypso.loader.ILoaderFactory;
import org.kalypso.ogc.sensor.deegree.ObservationLinkHandler;
import org.kalypso.services.calcjob.CalcJobService;
import org.kalypso.services.factory.ServiceFactory;
import org.kalypso.services.factory.ServiceFactoryException;
import org.kalypso.util.pool.ResourcePool;
import org.kalypso.util.repository.DefaultRepositoryContainer;
import org.kalypso.util.repository.RepositorySpecification;
import org.opengis.cs.CS_CoordinateSystem;
import org.osgi.framework.BundleContext;

import sun.misc.BASE64Encoder;

/**
 * The main plugin class to be used in the desktop.
 */
public class KalypsoGisPlugin extends AbstractUIPlugin
{
  private static final String BUNDLE_NAME = KalypsoGisPlugin.class.getPackage().getName()
      + ".resources.KalypsoGisPluginResources";

  private static KalypsoGisPlugin m_plugin = null;

  private ResourceBundle m_resourceBundle = null;

  private CS_CoordinateSystem myCoordinateSystem = null;

  private final HashMap myPools = new HashMap();

  private final HashMap myLoaderFactories = new HashMap();

  private static final String POOL_PROPERTIES = "resources/pools.properties";

  private final Properties myPoolProperties = new Properties();

  private Properties m_ftpProperties;

  private final Properties m_zmlRepositoriesProperties = new Properties();

  private static final String ZML_REPOSITORIES_PROPERTIES = "resources/zml_repositories.properties";

  private DefaultRepositoryContainer m_tsRepositoryContainer = null;

  private RepositorySpecification[] m_repositoriesSpecification = null;

  private final SelectionIdProvider mySelectionIdProvider = new SelectionIdProvider();

  private CalcJobService m_calcJobService;

  private IURLConnectionFactory m_urlConnectionFactory;

  /**
   * The constructor.
   */
  public KalypsoGisPlugin()
  {
    m_plugin = this;

    configureLogger();
    configureProxy();
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
      myPoolProperties.load( this.getClass().getResourceAsStream( POOL_PROPERTIES ) );

      m_zmlRepositoriesProperties.load( getClass()
          .getResourceAsStream( ZML_REPOSITORIES_PROPERTIES ) );
      prepareRepositoriesSpecifications();
    }
    catch( final IOException e )
    {
      e.printStackTrace();
    }

    try
    {
      UIManager.setLookAndFeel( UIManager.getSystemLookAndFeelClassName() );
    }
    catch( Exception e1 )
    {
      e1.printStackTrace();
    }

    registerTypeHandler();
  }

  private void configureLogger()
  {
    final Logger logger = Logger.getLogger( "" );
    logger.setLevel( Level.INFO );

    final Handler[] handlers = logger.getHandlers();
    for( int i = 0; i < handlers.length; i++ )
    {
      final Handler handler = handlers[i];
      handler.setLevel( Level.FINER );
    }
  }

  /**
   * Helper: vorbereitet die Liste der zur Verfügung stehende Repositories.
   */
  private void prepareRepositoriesSpecifications()
  {
    String[] available = m_zmlRepositoriesProperties.getProperty( "available" ).split( "," );

    m_repositoriesSpecification = new RepositorySpecification[available.length];

    for( int i = 0; i < available.length; i++ )
      m_repositoriesSpecification[i] = new RepositorySpecification( available[i],
          m_zmlRepositoriesProperties.getProperty( available[i] ), m_zmlRepositoriesProperties
              .getProperty( available[i] + "_FACTORY" ) );
  }

  /**
   * Liefert die Liste der Konfigurierte und Zugreifbare Zeitreihen Repositories
   */
  public RepositorySpecification[] getRepositoriesSpecifications()
  {
    return m_repositoriesSpecification;
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

  public CalcJobService getCalcService()
  {
    if( m_calcJobService == null )
    {
      try
      {
        m_calcJobService = (CalcJobService)ServiceFactory.createService( "CalcJob.Default" );
      }
      catch( ServiceFactoryException e )
      {
        e.printStackTrace();
      }
    }

    return m_calcJobService;
  }

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
    System.setProperty( "proxySet", "true" );
    System.setProperty( "proxyHost", "172.16.0.1" );
    System.setProperty( "proxyPort", "8080" );
    String pw = "belger:LaufMensch";
    String epw = "Basic " + ( new BASE64Encoder() ).encode( pw.getBytes() );

    m_urlConnectionFactory = new URLConnectionFactory( "Proxy-Authorization", epw );
    NetWorker.setURLConnectionFactory( m_urlConnectionFactory );
  }

  public IURLConnectionFactory getURLConnectionFactory()
  {
    return m_urlConnectionFactory;
  }

  private class URLConnectionFactory implements IURLConnectionFactory
  {
    private final String m_property;

    private final String m_value;

    private URLConnectionFactory( String property, String value )
    {
      m_property = property;
      m_value = value;
    }

    private URLConnectionFactory()
    {
      m_property = null;
      m_value = null;
    }

    /**
     * @see org.deegree.tools.IURLConnectionFactory#createURLConnection(java.net.URL)
     */
    public URLConnection createURLConnection( URL url ) throws IOException
    {
      if( m_property == null )
        return url.openConnection();
      URLConnection connection = url.openConnection();
      connection.setRequestProperty( m_property, m_value );
      return connection;
    }
  }
}