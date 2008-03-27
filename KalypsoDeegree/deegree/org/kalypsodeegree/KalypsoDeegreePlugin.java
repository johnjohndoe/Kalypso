package org.kalypsodeegree;

import java.io.File;
import java.util.logging.Logger;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Plugin;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.ui.preferences.ScopedPreferenceStore;
import org.kalypso.preferences.IKalypsoDeegreePreferences;
import org.kalypso.transformation.CRSHelper;
import org.kalypsodeegree_impl.graphics.sld.DefaultStyleFactory;
import org.osgi.framework.BundleContext;

public class KalypsoDeegreePlugin extends Plugin
{
  /**
   * The logger.
   */
  private static final Logger LOGGER = Logger.getLogger( KalypsoDeegreePlugin.class.getName() );

  /**
   * The shared instance.
   */
  private static KalypsoDeegreePlugin m_plugin;

  /**
   * The default style factory.
   */
  private DefaultStyleFactory m_defaultStyleFactory;

  /**
   * Storage for preferences.
   */
  private ScopedPreferenceStore m_preferenceStore;

  /**
   * The coordinate system.
   */
  private String m_coordinateSystem;

  /**
   * The constructor.
   */
  public KalypsoDeegreePlugin( )
  {
    super();

    m_plugin = this;
    m_defaultStyleFactory = null;
    m_preferenceStore = null;
    m_coordinateSystem = null;
  }

  /**
   * @see org.eclipse.core.runtime.Plugin#start(org.osgi.framework.BundleContext)
   */
  @Override
  public void start( final BundleContext context ) throws Exception
  {
    super.start( context );
  }

  /**
   * @see org.eclipse.core.runtime.Plugin#stop(org.osgi.framework.BundleContext)
   */
  @Override
  public void stop( final BundleContext context ) throws Exception
  {
    savePluginPreferences();

    m_plugin = null;
    m_defaultStyleFactory = null;
    m_preferenceStore = null;
    m_coordinateSystem = null;

    super.stop( context );
  }

  /**
   * Returns the shared instance.
   */
  public static KalypsoDeegreePlugin getDefault( )
  {
    return m_plugin;
  }

  private void configureDefaultStyleFactory( )
  {
    final IPath stateLocation = getStateLocation();
    final File defaultStyleDir = new File( stateLocation.toFile(), "defaultStyles" );
    if( !defaultStyleDir.exists() )
    {
      defaultStyleDir.mkdir();
    }
    try
    {
      m_defaultStyleFactory = DefaultStyleFactory.getFactory( defaultStyleDir.getAbsolutePath() );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      KalypsoDeegreePlugin.LOGGER.warning( "Default style location was not created, DefaultStyleFactory is not available." );
    }
  }

  public static DefaultStyleFactory getDefaultStyleFactory( )
  {
    final KalypsoDeegreePlugin plugin = KalypsoDeegreePlugin.getDefault();
    if( plugin.m_defaultStyleFactory == null )
    {
      plugin.configureDefaultStyleFactory();
    }
    return plugin.m_defaultStyleFactory;
  }

  /**
   * This function returns the coordinate system set in the preferences.
   * 
   * @return The coordinate system.
   */
  public String getCoordinateSystem( )
  {
    if( m_coordinateSystem == null )
    {
      String crsName = getPluginPreferences().getString( IKalypsoDeegreePreferences.DEFAULT_CRS_SETTING );
      boolean knownCRS = CRSHelper.isKnownCRS( crsName );

      if( crsName == null || !knownCRS )
      {
        getPluginPreferences().setValue( IKalypsoDeegreePreferences.DEFAULT_CRS_SETTING, IKalypsoDeegreePreferences.DEFAULT_CRS_VALUE );
        System.out.println( "CRS \"" + crsName + "\" in preferences is unknown. setting preferences to CRS \"" + IKalypsoDeegreePreferences.DEFAULT_CRS_VALUE + "\"" );
        crsName = IKalypsoDeegreePreferences.DEFAULT_CRS_VALUE;
      }

      m_coordinateSystem = crsName;
    }

    return m_coordinateSystem;
  }

  /**
   * Copied from {@link org.eclipse.ui.plugin.AbstractUIPlugin}.
   * <p>
   * Returns the preference store for this UI plug-in. This preference store is used to hold persistent settings for
   * this plug-in in the context of a workbench. Some of these settings will be user controlled, whereas others may be
   * internal setting that are never exposed to the user.
   * <p>
   * If an error occurs reading the preference store, an empty preference store is quietly created, initialized with
   * defaults, and returned.
   * </p>
   * <p>
   * <strong>NOTE:</strong> As of Eclipse 3.1 this method is no longer referring to the core runtime compatibility
   * layer and so plug-ins relying on Plugin#initializeDefaultPreferences will have to access the compatibility layer
   * themselves.
   * </p>
   * 
   * @return the preference store
   */
  public IPreferenceStore getPreferenceStore( )
  {
    /* Create the preference store lazily. */
    if( m_preferenceStore == null )
      m_preferenceStore = new ScopedPreferenceStore( new InstanceScope(), getBundle().getSymbolicName() );

    return m_preferenceStore;
  }
}