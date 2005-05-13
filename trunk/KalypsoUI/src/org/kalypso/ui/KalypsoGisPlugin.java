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
package org.kalypso.ui;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.Authenticator;
import java.net.PasswordAuthentication;
import java.net.URL;
import java.net.URLStreamHandler;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.MissingResourceException;
import java.util.Properties;
import java.util.ResourceBundle;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.UIManager;
import javax.xml.rpc.ServiceException;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.kalypso.java.lang.reflect.ClassUtilities;
import org.kalypso.java.net.IUrlCatalog;
import org.kalypso.java.net.MultiUrlCatalog;
import org.kalypso.java.net.PropertyUrlCatalog;
import org.kalypso.loader.DefaultLoaderFactory;
import org.kalypso.loader.ILoaderFactory;
import org.kalypso.ogc.gml.table.celleditors.DefaultFeatureModifierFactory;
import org.kalypso.ogc.gml.table.celleditors.IFeatureModifierFactory;
import org.kalypso.ogc.gml.typehandler.DiagramTypeHandler;
import org.kalypso.ogc.sensor.deegree.ObservationLinkHandler;
import org.kalypso.ogc.sensor.view.ObservationCache;
import org.kalypso.repository.container.DefaultRepositoryContainer;
import org.kalypso.repository.container.IRepositoryContainer;
import org.kalypso.services.ProxyFactory;
import org.kalypso.services.ocs.OcsURLStreamHandler;
import org.kalypso.services.ocs.repository.ServiceRepositoryObservation;
import org.kalypso.services.proxy.ICalculationService;
import org.kalypso.services.proxy.IObservationService;
import org.kalypso.services.proxy.IUserService;
import org.kalypso.ui.preferences.IKalypsoPreferences;
import org.kalypso.users.User;
import org.kalypso.util.pool.ResourcePool;
import org.kalypsodeegree_impl.extension.ITypeRegistry;
import org.kalypsodeegree_impl.extension.TypeRegistrySingleton;
import org.kalypsodeegree_impl.gml.schema.GMLSchemaCatalog;
import org.kalypsodeegree_impl.graphics.sld.DefaultStyleFactory;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactoryFull;
import org.kalypsodeegree_impl.model.cv.RangeSetTypeHandler;
import org.kalypsodeegree_impl.model.cv.RectifiedGridDomainTypeHandler;
import org.opengis.cs.CS_CoordinateSystem;
import org.osgi.framework.BundleContext;
import org.osgi.service.url.URLConstants;
import org.osgi.service.url.URLStreamHandlerService;

/**
 * The main plugin class to be used in the desktop.
 */
public class KalypsoGisPlugin extends AbstractUIPlugin implements
    IPropertyChangeListener
{
  private static final String SCHEMA_CATALOG = "SCHEMA_CATALOG_URL";
  private static final String PROGNOSE_MODELLIST = "PROGNOSE_MODELLIST_URL";
  private static final String MODELL_REPOSITORY = "MODELL_REPOSITORY";
  
  private static final Logger LOGGER = Logger.getLogger( KalypsoGisPlugin.class
      .getName() );

  private static final String BUNDLE_NAME = KalypsoGisPlugin.class.getPackage()
      .getName()
      + ".resources.KalypsoGisPluginResources"; //$NON-NLS-N$

  /** location of the pool properties file */
  private static final String POOL_PROPERTIES = "resources/pools.properties"; //$NON-NLS-N$

  private static KalypsoGisPlugin THE_PLUGIN = null;

  private ResourceBundle m_resourceBundle = null;

  private CS_CoordinateSystem myCoordinateSystem = null;

  /** Manages the list of repositories. */
  private IRepositoryContainer m_tsRepositoryContainer = null;

  private final SelectionIdProvider mySelectionIdProvider = new SelectionIdProvider();

  /** factory for webservice proxy for the kalypso client */
  private ProxyFactory m_proxyFactory;

  /**
   * Configuration of this client. The configuration is build using the system
   * properties as well as remote properties defined on the potential
   * kalypso-servers.
   * <p>
   * The properties is basically a hashmap mapping string keys to string
   * representation of URLs or other kind of objects.
   */
  private final Properties m_mainConf = new Properties();

  private ResourcePool m_pool;

  private final Properties m_poolproperties = new Properties();

  private ILoaderFactory m_loaderFactory;

  private DefaultFeatureModifierFactory m_defaultFeatureControlFactory;

  //    private static final String DEFAULT_CRS = "EPSG:4326";
  private static final String DEFAULT_CRS = "EPSG:31469";

  private DefaultStyleFactory m_defaultStyleFactory;

  private IProject m_defaultStyleProject;

  /**
   * The current user that has successfully logged into kalypso. Usually an
   * application is responsible for initialising the current user using setUser(
   * user )
   */
  private User m_user;

  // TODO put definition in preferences dialog
  // TODO add crs attribute in boundingbox of *.gmt files
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
   * Loads the client configuration from the various server that were configured
   * in the kalypso plugin preferences page.
   * 
   * @param mainConf
   */
  private void configure( final Properties mainConf )
  {
    // put system properties
    mainConf.putAll( System.getProperties() );

    final String confUrls = getPluginPreferences().getString(
        IKalypsoPreferences.CLIENT_CONF_URLS );

    if( confUrls == null )
    {
      MessageDialog.openWarning( getWorkbench().getDisplay().getActiveShell(),
          "Konfiguration für Kalypso",
          "Keine Serverkonfiguration vorhanden. Funktionalität eingeschränkt." );
      return;
    }

    // try to laod conf file
    final String[] locs = confUrls.split( "," );

    // for each of the locations, fetch configuration and merge them with main
    // conf
    for( int i = 0; i < locs.length; i++ )
    {
      InputStream stream = null;

      try
      {
        final URL url = new URL( locs[i] );

        stream = new BufferedInputStream( url.openStream() );

        final Properties conf = new Properties();
        conf.load( stream );

        // merge the conf
        for( final Iterator it = conf.keySet().iterator(); it.hasNext(); )
        {
          final String key = (String)it.next();

          // TRICKY: the convention of the conf-file says that if the
          // property ends with '_URL' then its value is either a relative
          // or absolute URL. In the case of relative URL (relative to the
          // conf-file), we need to resolve them here because it's the only
          // place where we know the URL of the conf-file.
          final String value;
          if( key != null && key.endsWith( "URL" ) )
            value = new URL( url, conf.getProperty( key ) ).toString();
          else
            value = conf.getProperty( key );

          if( m_mainConf.containsKey( key ) )
          {
            String prop = m_mainConf.getProperty( key );
            prop += ',' + value;

            m_mainConf.put( key, prop );
          }
          else
            m_mainConf.put( key, value );
        }
      }
      catch( final Exception e ) // gen ex for simplicity
      {
        // do nothing, try with next location
        e.printStackTrace();

        String msg = "Konnte Konfigurationsdatei nicht laden: " + locs[i]
            + "\n";

        if( i == locs.length - 1 )
          msg += "Serverkonfiguration konnte nicht gefunden werden! Stelle Sie sicher dass mindestens ein Server zur Verfügung steht.\nAlterntiv, prüfen Sie die Liste der Server in den Applikationseinstellungen (Kalypso Seite).";
        else
          msg += "Es wird versucht, eine alternative Konfigurationsdatei zu laden.\nNächster Versuch:"
              + locs[i + 1];

        LOGGER.warning( msg );
      }
      finally
      {
        IOUtils.closeQuietly( stream );
      }
    }
  }

  /**
   * Loads the pool configuration
   * 
   * @throws IOException
   */
  private void configurePool() throws IOException
  {
    m_poolproperties.load( this.getClass()
        .getResourceAsStream( POOL_PROPERTIES ) );
  }

  /**
   * Sets service proxy factory specific properties and creates the proxy
   * factory object.
   * 
   * @param mainConf
   */
  private void configureServiceProxyFactory( final Properties mainConf )
  {
    // this is the base classname (actually just package name) of all the
    // kalypso service proxies
    mainConf.setProperty( ProxyFactory.KALYPSO_PROXY_BASE,
        "org.kalypso.services.proxy" );

    m_proxyFactory = new ProxyFactory( mainConf );
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
   * Eclipse comes with its own StreamHandler proxy. So we just need to say
   * which Handler to use for the protocol we can cover.
   * <p>
   * Following handlers are registered:
   * <ul>
   * <li>OcsURLStreamHandler for 'kalypso-ocs' protocol. Handles Observation
   * WebService urls.</li>
   * <li>TODO: insert your own handlers here...</li>
   * </ul>
   * 
   * @param context
   */
  private void configureURLStreamHandler( final BundleContext context )
  {
    // register the observation webservice url stream handler
    registerUrlStreamHandler( context, ServiceRepositoryObservation.SCHEME_OCS,
        new OcsURLStreamHandler() );
    registerUrlStreamHandler( context, CalculationSchemaStreamHandler.PROTOCOL,
        new CalculationSchemaStreamHandler() );
  }

  private void registerUrlStreamHandler( final BundleContext context,
      final String scheme, final URLStreamHandler handler )
  {
    final Hashtable properties = new Hashtable( 1 );
    properties.put( URLConstants.URL_HANDLER_PROTOCOL, new String[]
    { scheme } );
    context.registerService( URLStreamHandlerService.class.getName(), handler,
        properties );
  }

  /**
   * @return Kalypso WebService ProxyFactory
   */
  public ProxyFactory getServiceProxyFactory()
  {
    return m_proxyFactory;
  }

  /**
   * Convenience method that returns the calculation service proxy
   * 
   * @throws ServiceException
   */
  public ICalculationService getCalculationServiceProxy()
      throws ServiceException
  {
    // TODO: maybe refator, so that m_proxyFactory is never null, if not, order of call to configure...() is importent
    return (ICalculationService)m_proxyFactory.getProxy( "Kalypso_CalculationService",
        ClassUtilities.getOnlyClassName( ICalculationService.class ) );
  }

  /**
   * Convenience method that returns the observation service proxy
   * 
   * @return WebService proxy for the IObservationService
   * 
   * @throws ServiceException
   */
  public IObservationService getObservationServiceProxy()
      throws ServiceException
  {
    return (IObservationService)m_proxyFactory.getProxy(
        "Kalypso_ObservationService", ClassUtilities
            .getOnlyClassName( IObservationService.class ) );
  }

  /**
   * Convenience method that returns the user service proxy
   * 
   * @return WebService proxy for the IUserService
   * 
   * @throws ServiceException
   */
  public IUserService getUserServiceProxy() throws ServiceException
  {
    return (IUserService)m_proxyFactory.getProxy( "Kalypso_UserService",
        ClassUtilities.getOnlyClassName( IUserService.class ) );
  }

  public ILoaderFactory getLoaderFactory()
  {
    if( m_loaderFactory == null )
      m_loaderFactory = new DefaultLoaderFactory( m_poolproperties, getClass()
          .getClassLoader() );

    return m_loaderFactory;
  }

  public DefaultStyleFactory getDefaultStyleFactory()
  {

    if( m_defaultStyleFactory == null )
    {
      try
      {
        String dir = getPluginPreferences().getString(
            IKalypsoPreferences.DEFAULT_STYLE_DIRECTORY );
        IWorkspace workspace = ResourcesPlugin.getWorkspace();
        IWorkspaceRoot root = workspace.getRoot();
        if( root.getLocation().isValidPath( dir ) )
        {
          m_defaultStyleProject = root.getProject( dir );
          if( !m_defaultStyleProject.exists() )
            m_defaultStyleProject.create( null );
          m_defaultStyleProject.open( null );
        }
        String string = m_defaultStyleProject.getLocation().toFile().toString();
        m_defaultStyleFactory = DefaultStyleFactory.getFactory( string );
      }
      catch( Exception e )
      {
        MessageDialog
            .openError(
                null,
                "Default Style Factory",
                "Default style folder was not created, DefaultStyleFactory is not available.\nCheck your Kalypso preferences" );
        return null;
      }
    }
    return m_defaultStyleFactory;
  }

  public IProject getDefaultStyleFactoryWorkspaceLocation()
  {
    return m_defaultStyleProject;
  }

  public ResourcePool getPool()
  {
    if( m_pool == null )
      m_pool = new ResourcePool( getLoaderFactory() );

    return m_pool;
  }

  /**
   * This method is called upon plug-in activation
   * 
   * @param context
   * @throws Exception
   */
  public void start( final BundleContext context ) throws Exception
  {
    super.start( context );

    try
    {
      m_mainConf.clear();

      configure( m_mainConf );
      configureProxy();
      configurePool();
      configureServiceProxyFactory( m_mainConf );
      configureURLStreamHandler( context );
      // muss NACH dem proxy und dem streamHandler konfiguriert werden!
      configureSchemaCatalog();

      getPreferenceStore().addPropertyChangeListener( this );
    }
    catch( final IOException e )
    {
      e.printStackTrace();
    }
  }

  private void configureSchemaCatalog()
  {
    final Properties catalog = new Properties();
    InputStream is = null;
    String catalogLocation = null;
    URL url = null;
    try
    {
      catalogLocation = m_mainConf.getProperty( SCHEMA_CATALOG );
      if( catalogLocation == null )
        return; // finally wird noch ausgeführt...

      LOGGER.info( SCHEMA_CATALOG + " in Kalypso.ini gefunden." );
      url = new URL( catalogLocation );
      is = new BufferedInputStream( url.openStream() );

      catalog.load( is );
      is.close();
    }
    catch( final Exception e )
    {
      // exceptions ignorieren: nicht schlimm, Eintrag ist optional
      LOGGER.info( SCHEMA_CATALOG + " in kalypso-client.ini nicht vorhanden. Schemas werden vom Rechendienst abgeholt." );
    }
    finally
    {
      IOUtils.closeQuietly( is );

      // cache immer initialisieren, zur Not auch leer, sonst geht gar nichts.
      final PropertyUrlCatalog serverUrlCatalog = new PropertyUrlCatalog( url,
          catalog );
      final IUrlCatalog calcCatalog = new CalcServiceCatalog();
      final IUrlCatalog theCatalog = new MultiUrlCatalog( new IUrlCatalog[]
      {
          serverUrlCatalog,
          calcCatalog } );

      final IPath stateLocation = getStateLocation();
      final File cacheDir = new File( stateLocation.toFile(), "schemaCache" );
      cacheDir.mkdir();

      GMLSchemaCatalog.init( theCatalog, cacheDir );
    }
  }

  /**
   * This method is called when the plug-in is stopped
   * 
   * @param context
   * @throws Exception
   */
  public void stop( final BundleContext context ) throws Exception
  {
    super.stop( context );

    getPreferenceStore().removePropertyChangeListener( this );

    // clear the observation cache
    ObservationCache.clearCache();

    if( m_tsRepositoryContainer != null )
      m_tsRepositoryContainer.dispose();
  }

  public static String getId()
  {
    return getDefault().getBundle().getSymbolicName();
  }

  /**
   * Returns the shared instance.
   * 
   * @return singleton
   */
  public static KalypsoGisPlugin getDefault()
  {
    // m_plugin should be set in the constructor
    if( THE_PLUGIN == null )
      throw new NullPointerException( "Plugin Kalypso noch nicht instanziert!" );

    return THE_PLUGIN;
  }

  /**
   * @param key
   * @return string from the plugin's resource bundle, or 'key' if not found.
   */
  public static String getResourceString( final String key )
  {
    final ResourceBundle bundle = KalypsoGisPlugin.getDefault()
        .getResourceBundle();
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
   * @return plugin's resource bundle
   */
  public ResourceBundle getResourceBundle()
  {
    return m_resourceBundle;
  }

  public String getLang()
  {
    return getPluginPreferences().getString( IKalypsoPreferences.LANGUAGE );
  }

  public CS_CoordinateSystem getCoordinatesSystem()
  {
    if( myCoordinateSystem == null )
    {
      String crsName = getPluginPreferences().getString(
          IKalypsoPreferences.GLOBAL_CRS );

      final ConvenienceCSFactoryFull csFac = new ConvenienceCSFactoryFull();
      if( crsName == null || !csFac.isKnownCS( crsName ) )
      {
        getPluginPreferences().setValue( IKalypsoPreferences.GLOBAL_CRS,
            DEFAULT_CRS );
        System.out.println( "CRS \"" + crsName
            + "\" in preferences is unknown. setting preferences to CRS \""
            + DEFAULT_CRS + "\"" );
        crsName = DEFAULT_CRS;
      }
      myCoordinateSystem = org.kalypsodeegree_impl.model.cs.Adapters
          .getDefault().export( csFac.getCSByName( crsName ) );
    }
    return myCoordinateSystem;
  }

  public IRepositoryContainer getRepositoryContainer()
  {
    if( m_tsRepositoryContainer == null )
      m_tsRepositoryContainer = new DefaultRepositoryContainer();

    return m_tsRepositoryContainer;
  }

  public Properties getDefaultRepositoryProperties()
  {
    final Properties props = new Properties();

    // set all known properties for repository
    final String value = getPluginPreferences().getString(
        IKalypsoPreferences.NUMBER_OF_DAYS );
    props.setProperty( IKalypsoPreferences.NUMBER_OF_DAYS, value );

    return props;
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
      // TODO: make new NA-project and move registration to it
      registry.registerTypeHandler( new DiagramTypeHandler() );
      registry.registerTypeHandler( new RangeSetTypeHandler() );
      registry.registerTypeHandler( new RectifiedGridDomainTypeHandler() );
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
            IKalypsoPreferences.HTTP_PROXY_USER ), getPluginPreferences()
            .getString( IKalypsoPreferences.HTTP_PROXY_PASS ).toCharArray() );
      }
    } );
  }

  public static Status createErrorStatus( final String message,
      final Throwable cause )
  {
    String msg = message;
    if( cause != null && cause.getLocalizedMessage() != null )
      msg += ":\n\r" + cause.getLocalizedMessage();

    return new Status( IStatus.ERROR, getId(), 0, msg, cause );
  }

  /**
   * @see org.eclipse.jface.util.IPropertyChangeListener#propertyChange(org.eclipse.jface.util.PropertyChangeEvent)
   */
  public void propertyChange( final PropertyChangeEvent event )
  {
    // reconfigure plugin
    // TOOD: change of proxy etc. is not handled
    // maybe do some refactoring with start() and this method
    if( event.getProperty().equals( IKalypsoPreferences.CLIENT_CONF_URLS ) )
    {
      m_mainConf.clear();
      configure( m_mainConf );
      configureServiceProxyFactory( m_mainConf );
      configureSchemaCatalog();
    }
  }

  public IFeatureModifierFactory createFeatureTypeCellEditorFactory()
  {
    if( m_defaultFeatureControlFactory == null )
      m_defaultFeatureControlFactory = new DefaultFeatureModifierFactory();
    return m_defaultFeatureControlFactory;
  }

  public URL getModellistLocation()
  {
    try
    {
      final String location = m_mainConf.getProperty( PROGNOSE_MODELLIST,
          null );
      if( location == null )
        return null;

      final String[] locations = location.split( "," );
      if( locations.length == 0 )
        return null;

      return new URL( locations[0] );
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      return null;
    }
  }

  public File getServerModelRoot()
  {
    final String location = m_mainConf.getProperty( MODELL_REPOSITORY, null );
    if( location == null )
      return null;

    final String[] locations = location.split( "," );
    if( locations.length == 0 )
      return null;

    try
    {
      return new File( locations[0] );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new IllegalStateException( e.getLocalizedMessage() );
    }
  }

  public User getUser()
  {
    if( m_user == null )
      throw new IllegalStateException();

    return m_user;
  }

  public void setUser( final User user )
  {
    m_user = user;
  }
}