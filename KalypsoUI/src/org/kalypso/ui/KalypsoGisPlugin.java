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
import java.net.URL;
import java.util.Properties;
import java.util.TimeZone;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.UIManager;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Preferences;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.kalypso.commons.eclipse.core.runtime.PluginImageProvider;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.core.runtime.TempFileUtilities;
import org.kalypso.contribs.java.net.IUrlCatalog;
import org.kalypso.contribs.java.net.PropertyUrlCatalog;
import org.kalypso.core.client.KalypsoServiceCoreClientPlugin;
import org.kalypso.i18n.Messages;
import org.kalypso.loader.DefaultLoaderFactory;
import org.kalypso.loader.ILoaderFactory;
import org.kalypso.ogc.gml.dict.DictionaryCatalog;
import org.kalypso.ogc.gml.table.celleditors.DefaultFeatureModifierFactory;
import org.kalypso.ogc.gml.table.celleditors.IFeatureModifierFactory;
import org.kalypso.ogc.sensor.cache.ObservationCache;
import org.kalypso.repository.container.DefaultRepositoryContainer;
import org.kalypso.repository.container.IRepositoryContainer;
import org.kalypso.ui.preferences.IKalypsoPreferences;
import org.kalypso.util.pool.ResourcePool;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree_impl.graphics.sld.DefaultStyleFactory;
import org.osgi.framework.BundleContext;

/**
 * The main plugin class to be used in the desktop.
 */
public class KalypsoGisPlugin extends AbstractUIPlugin implements IPropertyChangeListener
{
  private static final String SCHEMA_CATALOG = "SCHEMA_CATALOG_URL"; //$NON-NLS-1$

  private static final String MODELL_REPOSITORY = "MODELL_REPOSITORY"; //$NON-NLS-1$

  private static final Logger LOGGER = Logger.getLogger( KalypsoGisPlugin.class.getName() );

  /** location of the pool properties file */
  private static final String POOL_PROPERTIES = "resources/pools.properties"; //$NON-NLS-1$

  private static KalypsoGisPlugin THE_PLUGIN = null;

  /** Manages the list of repositories. */
  private IRepositoryContainer m_tsRepositoryContainer = null;

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

  private DictionaryCatalog m_dictionaryCatalog;

  private PluginImageProvider m_imgProvider = null;

  /**
   * The constructor. Manages the configuration of the kalypso client.
   */
  public KalypsoGisPlugin( )
  {
    KalypsoGisPlugin.THE_PLUGIN = this;

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
  }

  /**
   * Loads the client configuration from the various server that were configured in the kalypso plugin preferences page.
   */
  private void configure( final Properties mainConf )
  {
    // put system properties
    mainConf.putAll( System.getProperties() );

    // overwrite the user settings if list was provided as program argument or system property
    final String confUrls = System.getProperty( "kalypso.client-ini-locations", "" ); //$NON-NLS-1$ //$NON-NLS-2$

    if( confUrls.length() == 0 )
    {
      final IStatus warningStatus = StatusUtilities.createWarningStatus( Messages.getString( "org.kalypso.ui.KalypsoGisPlugin.6" ) ); //$NON-NLS-1$
      getLog().log( warningStatus );
    }

    // try to load conf file
    final String[] locs = confUrls.split( "," ); //$NON-NLS-1$

    // for each of the locations, fetch configuration and merge them with main
    // conf
    for( int i = 0; i < locs.length; i++ )
    {
      InputStream stream = null;

      final String location = locs[i].trim();
      if( location.length() == 0 )
      {
        continue;
      }

      try
      {
        final URL url = new URL( location );

        stream = new BufferedInputStream( url.openStream() );

        final Properties conf = new Properties();
        conf.load( stream );

        // merge the conf
        for( final Object element : conf.keySet() )
        {
          final String key = (String) element;

          // TRICKY: the convention of the conf-file says that if the
          // property ends with '_URL' then its value is either a relative
          // or absolute URL. In the case of relative URL (relative to the
          // conf-file), we need to resolve them here because it's the only
          // place where we know the URL of the conf-file.
          final String value;
          if( (key != null) && key.endsWith( "URL" ) ) //$NON-NLS-1$
          {
            value = new URL( url, conf.getProperty( key ) ).toString();
          }
          else
          {
            value = conf.getProperty( key );
          }

          if( m_mainConf.containsKey( key ) )
          {
            String prop = m_mainConf.getProperty( key );
            prop += ',' + value;

            m_mainConf.put( key, prop );
          }
          else
          {
            m_mainConf.put( key, value );
          }
        }
      }
      catch( final Exception e ) // gen ex for simplicity
      {
        // do nothing, try with next location
        // e.printStackTrace();

        String msg = Messages.getString( "org.kalypso.ui.KalypsoGisPlugin.9" ) + location + "\n"; //$NON-NLS-1$ //$NON-NLS-2$

        if( i == locs.length - 1 )
        {
          msg += Messages.getString( "org.kalypso.ui.KalypsoGisPlugin.11" ); //$NON-NLS-1$
        }
        else
        {
          msg += Messages.getString( "org.kalypso.ui.KalypsoGisPlugin.12" ) + locs[i + 1]; //$NON-NLS-1$
        }

        KalypsoGisPlugin.LOGGER.warning( msg );
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
  private void configurePool( ) throws IOException
  {
    m_poolproperties.load( this.getClass().getResourceAsStream( KalypsoGisPlugin.POOL_PROPERTIES ) );
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

  private void configureLogger( )
  {
    // TODO:REMOVE THIS: we should always use the eclipse logging mechanisms
    final Logger logger = Logger.getLogger( "org.kalypso" ); //$NON-NLS-1$
    logger.setLevel( Level.INFO );

    final Handler[] handlers = logger.getHandlers();
    for( final Handler handler : handlers )
    {
      handler.setLevel( Level.FINER );
    }
  }

  /**
   * Delete a list of temp dirs found in the properties file 'deletetempdir.properties'. This method is called on
   * plugin-startup to clean the specified directories.
   */
  private void deleteTempDirs( )
  {
    final Properties props = new Properties();
    InputStream ins = null;
    try
    {
      ins = getClass().getResourceAsStream( "resources/deletetempdir.properties" ); //$NON-NLS-1$
      props.load( ins );
      ins.close();

      final String pDirs = props.getProperty( "DELETE_STARTUP", "" ); //$NON-NLS-1$ //$NON-NLS-2$
      final String[] dirNames = pDirs.split( "," ); //$NON-NLS-1$
      for( final String element : dirNames )
      {
        TempFileUtilities.deleteTempDir( this, element );
      }
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

  public ILoaderFactory getLoaderFactory( )
  {
    if( m_loaderFactory == null )
    {
      m_loaderFactory = new DefaultLoaderFactory( m_poolproperties, getClass().getClassLoader() );
    }

    return m_loaderFactory;
  }

  /**
   * use {@link KalypsoDeegreePlugin#getDefaultStyleFactory()} instead
   */
  @Deprecated
  public static DefaultStyleFactory getDefaultStyleFactory( )
  {
    return KalypsoDeegreePlugin.getDefaultStyleFactory();
  }

  public ResourcePool getPool( )
  {
    if( m_pool == null )
    {
      m_pool = new ResourcePool( getLoaderFactory() );
    }

    return m_pool;
  }

  /**
   * This method is called upon plug-in activation
   */
  @Override
  public void start( final BundleContext context ) throws Exception
  {
    super.start( context );

    m_imgProvider = new PluginImageProvider( this );
    m_imgProvider.resetTmpFiles();
    configureLogger();

    try
    {
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
  private void reconfigure( ) throws IOException
  {
    m_mainConf.clear();

    configure( m_mainConf );
    configurePool();
    configureServiceProxyFactory( m_mainConf );

    // muss NACH dem streamHandler konfiguriert werden!
    // configureDefaultStyleFactory();

    deleteTempDirs();
  }

  public IUrlCatalog loadRemoteSchemaCatalog( )
  {
    final Properties catalog = new Properties();
    InputStream is = null;
    String catalogLocation = null;
    URL url = null;
    try
    {
      catalogLocation = m_mainConf.getProperty( KalypsoGisPlugin.SCHEMA_CATALOG );
      if( catalogLocation != null )
      {
        KalypsoGisPlugin.LOGGER.info( KalypsoGisPlugin.SCHEMA_CATALOG + Messages.getString( "org.kalypso.ui.KalypsoGisPlugin.18" ) ); //$NON-NLS-1$
        url = new URL( catalogLocation );
        is = new BufferedInputStream( url.openStream() );

        catalog.load( is );
        is.close();

        return new PropertyUrlCatalog( url, catalog );
      }
    }
    catch( final IOException e )
    {
      // exceptions ignorieren: nicht schlimm, Eintrag ist optional
      KalypsoGisPlugin.LOGGER.info( KalypsoGisPlugin.SCHEMA_CATALOG + Messages.getString( "org.kalypso.ui.KalypsoGisPlugin.19" ) ); //$NON-NLS-1$
    }
    finally
    {
      IOUtils.closeQuietly( is );
    }

    // If no catalog could be loaded, return an empty catalog
    return new PropertyUrlCatalog( null, catalog );
  }

  /**
   * This method is called when the plug-in is stopped
   * 
   * @param context
   * @throws Exception
   */
  @Override
  public void stop( final BundleContext context ) throws Exception
  {
    super.stop( context );

    getPreferenceStore().removePropertyChangeListener( this );

    // clear the observation cache
    ObservationCache.clearCache();

    // clear the default styles
    // KalypsoGisPlugin.m_defaultStyleFactory.clear();

    if( m_tsRepositoryContainer != null )
    {
      m_tsRepositoryContainer.dispose();
    }

    m_imgProvider.resetTmpFiles();
    m_imgProvider = null;

    m_dictionaryCatalog = null;
  }

  public static String getId( )
  {
    return KalypsoGisPlugin.getDefault().getBundle().getSymbolicName();
  }

  /**
   * Returns the shared instance.
   * 
   * @return singleton
   */
  public static KalypsoGisPlugin getDefault( )
  {
    // m_plugin should be set in the constructor
    if( KalypsoGisPlugin.THE_PLUGIN == null )
      throw new NullPointerException( Messages.getString( "org.kalypso.ui.KalypsoGisPlugin.20" ) ); //$NON-NLS-1$

    return KalypsoGisPlugin.THE_PLUGIN;
  }

  /**
   * @return the timeZone as defined in the KALYPSO preferences. If unknown, the JVM default timezone is returned.
   */
  public TimeZone getDisplayTimeZone( )
  {
    final Preferences preferences = KalypsoDeegreePlugin.getDefault().getPluginPreferences();

    final String timeZoneID = preferences.getString( IKalypsoPreferences.DISPLAY_TIMEZONE );
    try
    {
      return TimeZone.getTimeZone( timeZoneID );
    }
    catch( final Exception e )
    {
      KalypsoGisPlugin.LOGGER.warning( e.getLocalizedMessage() );
      KalypsoGisPlugin.LOGGER.warning( Messages.getString( "org.kalypso.ui.KalypsoGisPlugin.21" ) ); //$NON-NLS-1$

      return TimeZone.getDefault();
    }
  }

  /**
   * @deprecated Use {@link KalypsoCorePlugin#getCoordinatesSystem()}} instead.
   */
  @Deprecated
  public String getCoordinatesSystem( )
  {
    return KalypsoDeegreePlugin.getDefault().getCoordinateSystem();
  }

  public IRepositoryContainer getRepositoryContainer( )
  {
    if( m_tsRepositoryContainer == null )
    {
      m_tsRepositoryContainer = new DefaultRepositoryContainer();
    }

    return m_tsRepositoryContainer;
  }

  public int getDefaultMapSelectionID( )
  {
    return 0x1;
  }

  /**
   * @see org.eclipse.jface.util.IPropertyChangeListener#propertyChange(org.eclipse.jface.util.PropertyChangeEvent)
   */
  public void propertyChange( final PropertyChangeEvent event )
  {
  }

  public IFeatureModifierFactory createFeatureTypeCellEditorFactory( )
  {
    if( m_defaultFeatureControlFactory == null )
    {
      m_defaultFeatureControlFactory = new DefaultFeatureModifierFactory();
    }
    return m_defaultFeatureControlFactory;
  }

  public File getServerModelRoot( )
  {
    final String location = m_mainConf.getProperty( KalypsoGisPlugin.MODELL_REPOSITORY, null );
    if( location == null )
      return null;

    final String[] locations = location.split( "," ); //$NON-NLS-1$
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

  public static PluginImageProvider getImageProvider( )
  {
    return KalypsoGisPlugin.getDefault().m_imgProvider;
  }

  public static DictionaryCatalog getDictionaryCatalog( )
  {
    final KalypsoGisPlugin defaultPlugin = KalypsoGisPlugin.getDefault();
    if( defaultPlugin.m_dictionaryCatalog == null )
    {
      defaultPlugin.m_dictionaryCatalog = new DictionaryCatalog();
    }
    return defaultPlugin.m_dictionaryCatalog;
  }
}