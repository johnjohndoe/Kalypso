/**
 *
 */
package org.kalypso.kalypsomodel1d2d;

import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.kalypso.commons.eclipse.core.runtime.PluginImageProvider;
import org.osgi.framework.BundleContext;

/**
 * @author Gernot Belger
 */
public class KalypsoModel1D2DPlugin extends AbstractUIPlugin
{
  public static final String PLUGIN_ID = "org.kalypso.model1d2d"; //$NON-NLS-1$

  // The shared instance.
  private static KalypsoModel1D2DPlugin plugin;

  private PluginImageProvider m_imageProvider;

  public KalypsoModel1D2DPlugin( )
  {
    plugin = this;
  }

  /**
   * This method is called upon plug-in activation
   */
  @Override
  public void start( final BundleContext context ) throws Exception
  {
    super.start( context );

    m_imageProvider = new PluginImageProvider( this );
  }

  /**
   * This method is called when the plug-in is stopped
   */
  @Override
  public void stop( final BundleContext context ) throws Exception
  {
    super.stop( context );
    plugin = null;

    m_imageProvider.resetTmpFiles();
    m_imageProvider = null;
  }

  /**
   * Returns the shared instance.
   */
  public static KalypsoModel1D2DPlugin getDefault( )
  {
    return plugin;
  }

  public static PluginImageProvider getImageProvider( )
  {
    return getDefault().m_imageProvider;
  }

}
