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
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.MissingResourceException;
import java.util.Properties;
import java.util.ResourceBundle;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.UIManager;
import javax.xml.rpc.ServiceException;

import org.apache.commons.httpclient.Credentials;
import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.UsernamePasswordCredentials;
import org.apache.commons.io.IOUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.ui.internal.Workbench;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.kalypso.contribs.eclipse.core.runtime.TempFileUtilities;
import org.kalypso.contribs.java.lang.reflect.ClassUtilities;
import org.kalypso.contribs.java.net.IUrlCatalog;
import org.kalypso.contribs.java.net.MultiUrlCatalog;
import org.kalypso.contribs.java.net.PropertyUrlCatalog;
import org.kalypso.core.KalypsoStart;
import org.kalypso.core.client.KalypsoServiceCoreClientPlugin;
import org.kalypso.loader.DefaultLoaderFactory;
import org.kalypso.loader.ILoaderFactory;
import org.kalypso.ogc.gml.gui.GuiTypeRegistrySingleton;
import org.kalypso.ogc.gml.gui.ResourceFileGuiTypeHandler;
import org.kalypso.ogc.gml.gui.TimeseriesLinkGuiTypeHandler;
import org.kalypso.ogc.gml.gui.ZmlInlineGuiTypeHandler;
import org.kalypso.ogc.gml.schema.virtual.VirtualRasterFeatureTypePropertyHandler;
import org.kalypso.ogc.gml.table.celleditors.DefaultFeatureModifierFactory;
import org.kalypso.ogc.gml.table.celleditors.IFeatureModifierFactory;
import org.kalypso.ogc.gml.typehandler.DiagramTypeHandler;
import org.kalypso.ogc.gml.typehandler.GM_ObjectTypeHandler;
import org.kalypso.ogc.gml.typehandler.ResourceFileTypeHandler;
import org.kalypso.ogc.gml.typehandler.ZmlInlineTypeHandler;
import org.kalypso.ogc.sensor.deegree.ObservationLinkHandler;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.view.ObservationCache;
import org.kalypso.ogc.sensor.zml.ZmlURLConstants;
import org.kalypso.repository.container.DefaultRepositoryContainer;
import org.kalypso.repository.container.IRepositoryContainer;
import org.kalypso.services.calculation.ICalculationServiceProxyFactory;
import org.kalypso.services.ocs.OcsURLStreamHandler;
import org.kalypso.services.proxy.ICalculationService;
import org.kalypso.services.proxy.IObservationService;
import org.kalypso.ui.preferences.IKalypsoPreferences;
import org.kalypso.util.pool.ResourcePool;
import org.kalypsodeegree_impl.extension.ITypeRegistry;
import org.kalypsodeegree_impl.extension.MarshallingTypeRegistrySingleton;
import org.kalypsodeegree_impl.gml.schema.GMLSchemaCatalog;
import org.kalypsodeegree_impl.gml.schema.schemata.DeegreeUrlCatalog;
import org.kalypsodeegree_impl.gml.schema.schemata.UrlCatalogUpdateObservationMapping;
import org.kalypsodeegree_impl.gml.schema.virtual.VirtualFeatureTypeRegistry;
import org.kalypsodeegree_impl.graphics.sld.DefaultStyleFactory;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactoryFull;
import org.kalypsodeegree_impl.model.cv.RangeSetTypeHandler;
import org.kalypsodeegree_impl.model.cv.RectifiedGridDomainTypeHandler;
import org.kalypsodeegree_impl.tools.GeometryUtilities;
import org.opengis.cs.CS_CoordinateSystem;
import org.osgi.framework.BundleContext;
import org.osgi.service.url.URLConstants;
import org.osgi.service.url.URLStreamHandlerService;

/**
 * The main plugin class to be used in the desktop.
 */
public class KalypsoGisPlugin extends AbstractUIPlugin implements IPropertyChangeListener
{
  private static final String SCHEMA_CATALOG = "SCHEMA_CATALOG_URL";

  private static final String PROGNOSE_MODELLIST = "PROGNOSE_MODELLIST_URL";

  private static final String MODELL_REPOSITORY = "MODELL_REPOSITORY";

  private static final Logger LOGGER = Logger.getLogger( KalypsoGisPlugin.class.getName() );

  private static final String BUNDLE_NAME = KalypsoGisPlugin.class.getPackage().getName()
      + ".resources.KalypsoGisPluginResources"; //$NON-NLS-N$

  /** location of the pool properties file */
  private static final String POOL_PROPERTIES = "resources/pools.properties"; //$NON-NLS-N$

  private static KalypsoGisPlugin THE_PLUGIN = null;

  private ResourceBundle m_resourceBundle = null;

  private CS_CoordinateSystem myCoordinateSystem = null;

  /** Manages the list of repositories. */
  private IRepositoryContainer m_tsRepositoryContainer = null;

  private final SelectionIdProvider mySelectionIdProvider = new SelectionIdProvider();

  /**
   * Configuration of this client. The configuration is build using the system properties as well as remote properties
   * defined on the potential kalypso-servers.
   * <p>
   * The properties is basically a hashmap mapping string keys to string representation of URLs or other kind of
   * objects.
   */
  private final Properties m_mainConf = new Properties();

  private ResourcePool m_pool;

  private final Properties m_poolproperties = new Properties();

  private ILoaderFactory m_loaderFactory;

  private DefaultFeatureModifierFactory m_defaultFeatureControlFactory;

  //    private static final String DEFAULT_CRS = "EPSG:4326";
  private static final String DEFAULT_CRS = "EPSG:31469";

  private static DefaultStyleFactory m_defaultStyleFactory;

  /**
   * The local CaluclationServices, e.g. the ones createt from the extension point
   */
  private Map m_localCalcServices = null;

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
    final ITypeRegistry marshallingRegistry = MarshallingTypeRegistrySingleton.getTypeRegistry();
    final ITypeRegistry guiRegistry = GuiTypeRegistrySingleton.getTypeRegistry();
    registerTypeHandler( marshallingRegistry, guiRegistry );
    registerVirtualFeatureTypeHandler();
  }

  private void registerVirtualFeatureTypeHandler()
  {
    VirtualFeatureTypeRegistry instance = VirtualFeatureTypeRegistry.getInstance();
    instance.register( new VirtualRasterFeatureTypePropertyHandler() );
  }

  /**
   * Loads the client configuration from the various server that were configured in the kalypso plugin preferences page.
   */
  private void configure( final Properties mainConf )
  {
    // put system properties
    mainConf.putAll( System.getProperties() );

    // overwrite the user settings if list was provided as program argument
    if( KalypsoStart.SERVER_LOCATIONS != null )
      getPluginPreferences().setValue( IKalypsoPreferences.CLIENT_CONF_URLS, KalypsoStart.SERVER_LOCATIONS );
    
    final String confUrls = getPluginPreferences().getString( IKalypsoPreferences.CLIENT_CONF_URLS );

    if( confUrls == null )
    {
      MessageDialog.openWarning( getWorkbench().getDisplay().getActiveShell(), "Konfiguration f�r Kalypso",
          "Keine Serverkonfiguration vorhanden. Funktionalit�t eingeschr�nkt." );
      return;
    }

    // try to load conf file
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

        String msg = "Konnte Konfigurationsdatei nicht laden: " + locs[i] + "\n";

        if( i == locs.length - 1 )
          msg += "Serverkonfiguration konnte nicht gefunden werden! Stelle Sie sicher dass mindestens ein Server zur Verf�gung steht.\nAlterntiv, pr�fen Sie die Liste der Server in den Applikationseinstellungen (Kalypso Seite).";
        else
          msg += "Es wird versucht, eine alternative Konfigurationsdatei zu laden.\nN�chster Versuch:" + locs[i + 1];

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
    m_poolproperties.load( this.getClass().getResourceAsStream( POOL_PROPERTIES ) );
  }

  /**
   * Sets service proxy factory specific properties and creates the proxy factory object.
   * 
   * @param mainConf
   */
  private void configureServiceProxyFactory( final Properties mainConf )
  {
    KalypsoServiceCoreClientPlugin.getDefault().configureProxies( mainConf );
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
   * Eclipse comes with its own StreamHandler proxy. So we just need to say which Handler to use for the protocol we can
   * cover.
   * <p>
   * Following handlers are registered:
   * <ul>
   * <li>OcsURLStreamHandler for 'kalypso-ocs' protocol. Handles Observation WebService urls.</li>
   * <li>XXX: insert your own handlers here...</li>
   * </ul>
   */
  private void configureURLStreamHandler( final BundleContext context )
  {
    // register the observation webservice url stream handler
    registerUrlStreamHandler( context, ZmlURLConstants.SCHEME_OCS, new OcsURLStreamHandler() );
    registerUrlStreamHandler( context, CalculationSchemaStreamHandler.PROTOCOL, new CalculationSchemaStreamHandler() );
  }

  private void registerUrlStreamHandler( final BundleContext context, final String scheme,
      final URLStreamHandler handler )
  {
    final Hashtable properties = new Hashtable( 1 );
    properties.put( URLConstants.URL_HANDLER_PROTOCOL, new String[]
    { scheme } );
    context.registerService( URLStreamHandlerService.class.getName(), handler, properties );
  }

  /**
   * Delete a list of temp dirs found in the properties file 'deletetempdir.properties'. This method is called on
   * plugin-startup to clean the specified directories.
   */
  private void deleteTempDirs()
  {
    final Properties props = new Properties();
    InputStream ins = null;
    try
    {
      ins = getClass().getResourceAsStream( "resources/deletetempdir.properties" );
      props.load( ins );
      ins.close();

      final String pDirs = props.getProperty( "DELETE_STARTUP", "" );
      final String[] dirNames = pDirs.split( "," );
      for( int i = 0; i < dirNames.length; i++ )
        TempFileUtilities.deleteTempDir( this, dirNames[i] );
    }
    catch( final IOException e )
    {
      e.printStackTrace();
    }
    finally
    {
      IOUtils.closeQuietly( ins );
    }
  }

  /**
   * Convenience method that returns the calculation service proxies.
   * 
   * @return A map name (e.g. url) to {@link ICalculationService}
   */
  public Map getCalculationServiceProxies()
  {
    final Map proxies = new LinkedHashMap();

    // put lokal services first, so they will be taken in preference
    proxies.putAll( getLocalCalcServices() );

    try
    {
      final Map stubs = KalypsoServiceCoreClientPlugin.getDefault().getProxyFactory().getAllProxiesAsMap(
          "Kalypso_CalculationService", ClassUtilities.getOnlyClassName( ICalculationService.class ) );
      proxies.putAll( stubs );
    }
    catch( final ServiceException e )
    {
      // server steht nicht zur Verf�gung oder ist falsch konfiguriert
      // exception wird gefangen, damit man immer noch lokal rechnen kann
    }

    return proxies;
  }

  private Map getLocalCalcServices()
  {
    if( m_localCalcServices == null )
    {
      final Map proxies = new HashMap();

      // alle proxy-factories holen und erzeugen
      final IExtensionRegistry registry = Platform.getExtensionRegistry();
      final IExtensionPoint point = registry.getExtensionPoint( getId(), IKalypsoUIConstants.PL_CALCULATION_SERVICE );
      // ??
      // System.out.println( "extensionPoint available ?" );
      if( point == null )
      {
        //     System.out.println( "extensionPoint == null" );
        return null;
      }
      final IExtension[] extensions = point.getExtensions();
      for( int i = 0; i < extensions.length; i++ )
      {
        //    System.out.println( "Extension: >" + extensions[i].getLabel() + "<" );
        final IExtension extension = extensions[i];
        final IConfigurationElement[] configurationElements = extension.getConfigurationElements();
        for( int j = 0; j < configurationElements.length; j++ )
        {
          //       System.out.println( " ConfigElelemnt: >" + configurationElements[j].getName() + "<" );
          final IConfigurationElement element = configurationElements[j];
          try
          {
            final ICalculationServiceProxyFactory factory = (ICalculationServiceProxyFactory)element
                .createExecutableExtension( "class" );
            proxies.put( "" + j + "_local_service_" + ClassUtilities.getOnlyClassName( factory.getClass() ), factory
                .createService() );
          }
          catch( final CoreException e )
          {
            // just log
            e.printStackTrace();
          }
        }
      }

      m_localCalcServices = proxies;
    }

    return m_localCalcServices;
  }

  /**
   * Convenience method that returns the observation service proxy
   * 
   * @return WebService proxy for the IObservationService
   * 
   * @throws ServiceException
   */
  public IObservationService getObservationServiceProxy() throws ServiceException
  {
    return (IObservationService)KalypsoServiceCoreClientPlugin.getDefault().getProxyFactory().getAnyProxy(
        "Kalypso_ObservationService", ClassUtilities.getOnlyClassName( IObservationService.class ) );
  }

  public ILoaderFactory getLoaderFactory()
  {
    if( m_loaderFactory == null )
      m_loaderFactory = new DefaultLoaderFactory( m_poolproperties, getClass().getClassLoader() );

    return m_loaderFactory;
  }

  public static DefaultStyleFactory getDefaultStyleFactory()
  {
    return m_defaultStyleFactory;
  }

  public ResourcePool getPool()
  {
    if( m_pool == null )
      m_pool = new ResourcePool( getLoaderFactory() );

    return m_pool;
  }

  /**
   * This method is called upon plug-in activation
   */
  public void start( final BundleContext context ) throws Exception
  {
    super.start( context );

    try
    {
      configureURLStreamHandler( context );

      reconfigure();

      getPreferenceStore().addPropertyChangeListener( this );
    }
    catch( final IOException e )
    {
      e.printStackTrace();
    }
  }

  /**
   * Reconfigure the plugin according to server properties and more.
   */
  private void reconfigure() throws IOException
  {
    m_mainConf.clear();

    configure( m_mainConf );
    configureProxy();
    configurePool();
    configureServiceProxyFactory( m_mainConf );

    // muss NACH dem proxy und dem streamHandler konfiguriert werden!
    configureSchemaCatalog();
    configureDefaultStyleFactory();

    deleteTempDirs();
  }

  private void configureDefaultStyleFactory()
  {

    final IPath stateLocation = getStateLocation();
    final File defaultStyleDir = new File( stateLocation.toFile(), "defaultStyles" );
    if( !defaultStyleDir.exists() )
      defaultStyleDir.mkdir();
    try
    {
      m_defaultStyleFactory = DefaultStyleFactory.getFactory( defaultStyleDir.getAbsolutePath() );
    }
    catch( Exception e )
    {
      e.printStackTrace();
      LOGGER.warning( "Default style location was not created, DefaultStyleFactory is not available." );
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
        return; // finally wird noch ausgef�hrt...

      LOGGER.info( SCHEMA_CATALOG + " in Kalypso.ini gefunden." );
      url = new URL( catalogLocation );
      is = new BufferedInputStream( url.openStream() );

      catalog.load( is );
      is.close();
    }
    catch( final Exception e )
    {
      // exceptions ignorieren: nicht schlimm, Eintrag ist optional
      LOGGER
          .info( SCHEMA_CATALOG + " in kalypso-client.ini nicht vorhanden. Schemas werden vom Rechendienst abgeholt." );
    }
    finally
    {
      IOUtils.closeQuietly( is );

      // cache immer initialisieren, zur Not auch leer, sonst geht gar nichts.
      final PropertyUrlCatalog serverUrlCatalog = new PropertyUrlCatalog( url, catalog );
      final IUrlCatalog calcCatalog = new CalcServiceCatalog();

      // TODO wohin sonst
      final IUrlCatalog updateObs = new UrlCatalogUpdateObservationMapping();
      // TODO @Andreas wieso war dieser Katalog nicht mehr hier wird der Serverseitig hinzugef�gt?
      // der Client sollte diese auf jedenfall zur Verf�gung haben deshalb habe ich den wieder eingef�gt. (CK) 20.7.2005
      final IUrlCatalog deegreeCatalog = new DeegreeUrlCatalog();
      final IUrlCatalog theCatalog = new MultiUrlCatalog( new IUrlCatalog[]
      {
      // test
          //          naCatalog,
          //          twoDCatalog,

          updateObs, serverUrlCatalog, calcCatalog, deegreeCatalog } );

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

    // clear the default styles
    m_defaultStyleFactory.clear();

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
      String crsName = getPluginPreferences().getString( IKalypsoPreferences.GLOBAL_CRS );

      final ConvenienceCSFactoryFull csFac = new ConvenienceCSFactoryFull();
      if( crsName == null || !csFac.isKnownCS( crsName ) )
      {
        getPluginPreferences().setValue( IKalypsoPreferences.GLOBAL_CRS, DEFAULT_CRS );
        System.out.println( "CRS \"" + crsName + "\" in preferences is unknown. setting preferences to CRS \""
            + DEFAULT_CRS + "\"" );
        crsName = DEFAULT_CRS;
      }
      myCoordinateSystem = org.kalypsodeegree_impl.model.cs.Adapters.getDefault().export( csFac.getCSByName( crsName ) );
    }
    return myCoordinateSystem;
  }

  public IRepositoryContainer getRepositoryContainer()
  {
    if( m_tsRepositoryContainer == null )
      m_tsRepositoryContainer = new DefaultRepositoryContainer();

    return m_tsRepositoryContainer;
  }

  public SelectionIdProvider getSelectionIdProvider()
  {
    return mySelectionIdProvider;
  }

  public static void registerTypeHandler( ITypeRegistry marhallingRegistry, ITypeRegistry guiRegistry )
  {
    // changed this to public static void, so it can be called from JUnitTests also
    // really not nice :-(

    //    final ITypeRegistry registry = MarshallingTypeRegistrySingleton.getTypeRegistry();
    //    final ITypeRegistry guiRegistry = GuiTypeRegistrySingleton.getTypeRegistry();
    // TODO TODO TODO: refaktor this shit!

    try
    {
      // TODO: read TypeHandler from property-file
      marhallingRegistry.registerTypeHandler( new ObservationLinkHandler() );
      // TODO: make new NA-project and move registration to it
      // TODO delete next
      marhallingRegistry.registerTypeHandler( new DiagramTypeHandler() );

      marhallingRegistry.registerTypeHandler( new RangeSetTypeHandler() );
      marhallingRegistry.registerTypeHandler( new RectifiedGridDomainTypeHandler() );
      marhallingRegistry.registerTypeHandler( new ResourceFileTypeHandler() );

      guiRegistry.registerTypeHandler( new TimeseriesLinkGuiTypeHandler() );
      guiRegistry.registerTypeHandler( new ResourceFileGuiTypeHandler() );
      //register gml-geometry types
      marhallingRegistry.registerTypeHandler( new GM_ObjectTypeHandler( "PointPropertyType", GeometryUtilities
          .getPointClass() ) );
      marhallingRegistry.registerTypeHandler( new GM_ObjectTypeHandler( "MultiPointPropertyType", GeometryUtilities
          .getMultiPointClass() ) );

      marhallingRegistry.registerTypeHandler( new GM_ObjectTypeHandler( "LineStringPropertyType", GeometryUtilities
          .getLineStringClass() ) );
      marhallingRegistry.registerTypeHandler( new GM_ObjectTypeHandler( "MultiLineStringPropertyType",
          GeometryUtilities.getMultiLineStringClass() ) );

      marhallingRegistry.registerTypeHandler( new GM_ObjectTypeHandler( "PolygonPropertyType", GeometryUtilities
          .getPolygonClass() ) );
      marhallingRegistry.registerTypeHandler( new GM_ObjectTypeHandler( "MultiPolygonPropertyType", GeometryUtilities
          .getMultiPolygonClass() ) );

      marhallingRegistry.registerTypeHandler( new GM_ObjectTypeHandler( "GeometryPropertyType", GeometryUtilities
          .getUndefinedGeometryClass() ) );
      // TODO LinearRingPropertyType, BoxPropertyype, GeometryCollectionPropertyType

      // register inlines

      final String[] wvqAxis = new String[]
      { TimeserieConstants.TYPE_NORMNULL, TimeserieConstants.TYPE_VOLUME, TimeserieConstants.TYPE_RUNOFF };
      final String[] taAxis = new String[]
      { TimeserieConstants.TYPE_HOURS, TimeserieConstants.TYPE_NORM };
      final String[] wtKcLaiAxis = new String[]
      { TimeserieConstants.TYPE_DATE, TimeserieConstants.TYPE_LAI, TimeserieConstants.TYPE_WT, TimeserieConstants.TYPE_KC };
      final ZmlInlineTypeHandler wvqInline = new ZmlInlineTypeHandler( "ZmlInlineWVQType", wvqAxis, "WVQ" );
      final ZmlInlineTypeHandler taInline = new ZmlInlineTypeHandler( "ZmlInlineTAType", taAxis, "TA" );
      final ZmlInlineTypeHandler wtKcLaiInline = new ZmlInlineTypeHandler( "ZmlInlineIdealKcWtLaiType", wtKcLaiAxis,
          "KCWTLAI" );
      marhallingRegistry.registerTypeHandler( wvqInline );
      marhallingRegistry.registerTypeHandler( taInline );
      marhallingRegistry.registerTypeHandler( wtKcLaiInline );
      guiRegistry.registerTypeHandler( new ZmlInlineGuiTypeHandler( wvqInline ) );
      guiRegistry.registerTypeHandler( new ZmlInlineGuiTypeHandler( taInline ) );
      guiRegistry.registerTypeHandler( new ZmlInlineGuiTypeHandler( wtKcLaiInline ) );
    }
    catch( Exception e ) // generic exception caught for simplicity
    {
      e.printStackTrace();

      MessageDialog.openError( Workbench.getInstance().getDisplay().getActiveShell(), "Interne Applikationsfehler", e
          .getLocalizedMessage() );
    }
  }

  public int getDefaultMapSelectionID()
  {
    return 0x1;
  }

  private void configureProxy()
  {
    System.setProperty( "proxySet", getPluginPreferences().getString( IKalypsoPreferences.HTTP_PROXY_USE ) );
    System.setProperty( "proxyHost", getPluginPreferences().getString( IKalypsoPreferences.HTTP_PROXY_HOST ) );
    System.setProperty( "proxyPort", getPluginPreferences().getString( IKalypsoPreferences.HTTP_PROXY_PORT ) );

    Authenticator.setDefault( new Authenticator()
    {
      /**
       * @see java.net.Authenticator#getPasswordAuthentication()
       */
      protected PasswordAuthentication getPasswordAuthentication()
      {
        return new PasswordAuthentication( getPluginPreferences().getString( IKalypsoPreferences.HTTP_PROXY_USER ),
            getPluginPreferences().getString( IKalypsoPreferences.HTTP_PROXY_PASS ).toCharArray() );
      }
    } );
  }

  /**
   * Creates a configured http client. The configuration includes setting of proxy settings.
   * <p>
   * IMPORTANT: to use proxy-authentication, you must use the setDoAuthetication Mehtod of the HttpMehthod you are going
   * to use.
   * </p>
   * <p>
   * Example: <code>
   *    final HttpMethod method = new GetMethod( m_url.toString() );
   *    method.setDoAuthentication( true );
   * </code>
   * </p>
   */
  public HttpClient createConfiguredHttpClient( final int timeout )
  {
    final HttpClient client = new HttpClient();
    client.getState().setAuthenticationPreemptive( true );

    KalypsoGisPlugin.getDefault();

    if( Boolean.getBoolean( "proxySet" ) )
    {
      final String proxyHost = System.getProperty( "proxyHost" );
      final String proxyPort = System.getProperty( "proxyPort" );
      final String proxyUser = getPluginPreferences().getString( IKalypsoPreferences.HTTP_PROXY_USER );

      // todo: this always gets the empty string, but proxy connection is working anyways
      // what to do?
      final String proxyPwd = getPluginPreferences().getString( IKalypsoPreferences.HTTP_PROXY_PASS );

      final Credentials defaultcreds = new UsernamePasswordCredentials( proxyUser, proxyPwd );

      client.getState().setProxyCredentials( null, proxyHost, defaultcreds );
      client.getHostConfiguration().setProxy( proxyHost, Integer.parseInt( proxyPort ) );
    }

    client.setTimeout( timeout );
    return client;
  }

  /**
   * @see org.eclipse.jface.util.IPropertyChangeListener#propertyChange(org.eclipse.jface.util.PropertyChangeEvent)
   */
  public void propertyChange( final PropertyChangeEvent event )
  {
    if( event.getProperty().equals( IKalypsoPreferences.CLIENT_CONF_URLS ) )
    {
      try
      {
        reconfigure();
      }
      catch( IOException e )
      {
        e.printStackTrace();
      }
    }
    if( event.getProperty().equals( IKalypsoPreferences.HTTP_PROXY_HOST )
        || event.getProperty().equals( IKalypsoPreferences.HTTP_PROXY_PASS )
        || event.getProperty().equals( IKalypsoPreferences.HTTP_PROXY_PORT )
        || event.getProperty().equals( IKalypsoPreferences.HTTP_PROXY_USER )
        || event.getProperty().equals( IKalypsoPreferences.HTTP_PROXY_USE ) )
    {
      configureProxy();
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
      final String location = m_mainConf.getProperty( PROGNOSE_MODELLIST, null );
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
}