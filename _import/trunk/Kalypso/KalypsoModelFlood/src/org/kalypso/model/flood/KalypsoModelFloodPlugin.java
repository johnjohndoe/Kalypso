package org.kalypso.model.flood;

import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.kalypso.commons.eclipse.core.runtime.PluginImageProvider;
import org.osgi.framework.BundleContext;

/**
 * The activator class controls the plug-in life cycle
 */
public class KalypsoModelFloodPlugin extends AbstractUIPlugin
{
  // The plug-in ID
  public static final String PLUGIN_ID = "org.kalypso.model.flood"; //$NON-NLS-1$

  // The shared instance
  private static KalypsoModelFloodPlugin plugin;

  private PluginImageProvider m_imageProvider;

  /**
   * The constructor
   */
  public KalypsoModelFloodPlugin( )
  {
    plugin = this;
  }

  /**
   * @see org.eclipse.ui.plugin.AbstractUIPlugin#start(org.osgi.framework.BundleContext)
   */
  @Override
  public void start( BundleContext context ) throws Exception
  {
    super.start( context );

    // delete tmp images both on startup and shutdown
    m_imageProvider = new PluginImageProvider( this );
    m_imageProvider.resetTmpFiles();
  }

  /**
   * @see org.eclipse.ui.plugin.AbstractUIPlugin#stop(org.osgi.framework.BundleContext)
   */
  @Override
  public void stop( BundleContext context ) throws Exception
  {
    // delete tmp images both on startup and shutdown
    m_imageProvider.resetTmpFiles();
    m_imageProvider = null;

    plugin = null;
    super.stop( context );
  }

  /**
   * Returns the shared instance
   * 
   * @return the shared instance
   */
  public static KalypsoModelFloodPlugin getDefault( )
  {
    return plugin;
  }

  public static PluginImageProvider getImageProvider( )
  {
    return getDefault().m_imageProvider;
  }

}
