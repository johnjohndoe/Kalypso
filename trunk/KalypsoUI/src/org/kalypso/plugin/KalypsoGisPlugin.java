package org.kalypso.plugin;

import java.io.IOException;
import java.util.HashMap;
import java.util.MissingResourceException;
import java.util.Properties;
import java.util.ResourceBundle;

import org.apache.commons.pool.KeyedObjectPool;
import org.deegree_impl.model.cs.ConvenienceCSFactoryFull;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.kalypso.eclipse.jface.viewers.DefaultCellEditorFactory;
import org.kalypso.eclipse.jface.viewers.ICellEditorFactory;
import org.kalypso.util.loader.DefaultLoaderFactory;
import org.kalypso.util.loader.ILoaderFactory;
import org.kalypso.util.pool.KalypsoKeyedObjectPool;
import org.kalypso.util.pool.TypedKeyedPoolableObjectFactory;
import org.kalypso.util.repository.DefaultRepositoryContainer;
import org.kalypso.util.repository.RepositorySpecification;
import org.opengis.cs.CS_CoordinateSystem;
import org.osgi.framework.BundleContext;

/**
 * The main plugin class to be used in the desktop.
 */
public class KalypsoGisPlugin extends AbstractUIPlugin
{
  private static final String BUNDLE_NAME = KalypsoGisPlugin.class.getPackage().getName()
      + ".resources.KalypsoGisPluginResources";

  private static KalypsoGisPlugin m_plugin = null;

  private ResourceBundle m_resourceBundle = null;

  // TODO private OutputLogger m_outputLogger = null;

  private CS_CoordinateSystem myCoordinateSystem = null;
  
  private final HashMap myPools = new HashMap();
  
  private final HashMap myLoaderFactories = new HashMap();
  
  private final Properties myPoolProperties = new Properties();

  private static final String POOL_PROPERTIES = "resources/pools.properties";

  private DefaultRepositoryContainer m_tsRepositoryContainer = null;

  private Properties m_ftpProperties;
  
  private final Properties m_zmlRepositoriesProperties = new Properties();
  private static final String ZML_REPOSITORIES_PROPERTIES = "resources/zml_repositories.properties";

  private RepositorySpecification[] m_repositoriesSpecification = null;

  private ICellEditorFactory m_featureTypeCellEditorFactory; 

  private final SelectionIdProvider mySelectionIdProvider=new SelectionIdProvider();
  
  /**
   * The constructor.
   */
  public KalypsoGisPlugin()
  {
    m_plugin = this;
    
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
      
      m_zmlRepositoriesProperties.load( getClass().getResourceAsStream( ZML_REPOSITORIES_PROPERTIES ) );
      prepareRepositoriesSpecifications();
    }
    catch( final IOException e )
    {
      e.printStackTrace();
    }
    
  }

  private void prepareRepositoriesSpecifications()
  {
    String[] available = m_zmlRepositoriesProperties.getProperty( "available" ).split(",");
    
    m_repositoriesSpecification = new RepositorySpecification[ available.length ];
    
    for( int i = 0; i < available.length; i++ )
      m_repositoriesSpecification[i] = 
        new RepositorySpecification( available[i],
            m_zmlRepositoriesProperties.getProperty( available[i] ),
            m_zmlRepositoriesProperties.getProperty( available[i] + "_FACTORY" ) );
  }
  
  /**
   * Liefert die Liste der Konfigurierte und Zugreifbare Zeitreihen Repositories
   */
  public RepositorySpecification[] getRepositoriesSpecifications()
  {
    return m_repositoriesSpecification;
  }

  public ImageDescriptor imageDescriptor( final String imageFilePath )
  {
    return imageDescriptor( "org.kalypso.ui", imageFilePath );
  }

  public ImageDescriptor imageDescriptor( final String pluginID, final String imageFilePath )
  {
    return AbstractUIPlugin.imageDescriptorFromPlugin( pluginID, imageFilePath );
  }

//  TODO public OutputLogger getOutputLogger()
//  {
//    if( m_outputLogger == null )
//      m_outputLogger = new OutputLogger();
//
//    return m_outputLogger;
//  }

  private ILoaderFactory getLoaderFactory( final Class valueClass )
  {
    ILoaderFactory loaderFactory = (ILoaderFactory)myLoaderFactories.get( valueClass );
    
    if( loaderFactory == null )
    {
      final String propFilename = (String)myPoolProperties.get( valueClass.getName() );
      
      final Properties props = new Properties();
      try
      {
        props.load( KalypsoGisPlugin.class.getResourceAsStream(propFilename) );
      }
      catch( IOException e )
      {
        // TODO besser handeln
        e.printStackTrace();
      }
      
      loaderFactory = new DefaultLoaderFactory( props, this.getClass().getClassLoader() );
      
      myLoaderFactories.put( valueClass, loaderFactory);
    }
    
    return loaderFactory;
  }
  
  public KeyedObjectPool getPool( final Class valueClass )
  {
    KeyedObjectPool pool = (KeyedObjectPool)myPools.get( valueClass );
    if( pool == null )
    {
    //  pool = new StackKeyedObjectPool( new TypedKeyedPoolableObjectFactory( getLoaderFactory( valueClass ) ) );
      pool = new KalypsoKeyedObjectPool( new TypedKeyedPoolableObjectFactory( getLoaderFactory( valueClass ) ) );
      myPools.put(valueClass, pool);
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
    if(myCoordinateSystem==null)
    {
      ConvenienceCSFactoryFull csFac=new ConvenienceCSFactoryFull();
      myCoordinateSystem=org.deegree_impl.model.cs.Adapters.getDefault(  ).export(csFac.getCSByName("EPSG:4326"));
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
        m_ftpProperties.load( KalypsoGisPlugin.class.getResourceAsStream( "resources/featureTypeEditor.properties" ) );
      }
      catch( final IOException e )
      {
        e.printStackTrace();
      }
    }
    
    return m_ftpProperties;
  }

  public ICellEditorFactory getFeatureTypeCellEditorFactory()
  {
    if( m_featureTypeCellEditorFactory == null )
      m_featureTypeCellEditorFactory = new DefaultCellEditorFactory( getFeatureTypeCellEditorProperties(), this.getClass().getClassLoader() );
    
    return m_featureTypeCellEditorFactory;
  }
  
  public SelectionIdProvider getSelectionIdProvider()
  {
      return mySelectionIdProvider;
  }
}
