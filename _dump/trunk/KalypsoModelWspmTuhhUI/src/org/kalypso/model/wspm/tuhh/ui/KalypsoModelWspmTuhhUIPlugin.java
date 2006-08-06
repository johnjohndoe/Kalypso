package org.kalypso.model.wspm.tuhh.ui;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.kalypso.commons.eclipse.core.runtime.PluginImageProvider;
import org.osgi.framework.BundleContext;

/**
 * The main plugin class to be used in the desktop.
 */
public class KalypsoModelWspmTuhhUIPlugin extends AbstractUIPlugin
{
  // The shared instance.
  private static KalypsoModelWspmTuhhUIPlugin plugin;

  private PluginImageProvider m_imgProvider = null;

  /**
   * The constructor.
   */
  public KalypsoModelWspmTuhhUIPlugin( )
  {
    super();
    plugin = this;
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
  }

  /**
   * This method is called when the plug-in is stopped
   */
  @Override
  public void stop( final BundleContext context ) throws Exception
  {
    m_imgProvider.resetTmpFiles();
    m_imgProvider = null;

    super.stop( context );

    plugin = null;
  }

  /**
   * Returns the shared instance.
   */
  public static KalypsoModelWspmTuhhUIPlugin getDefault( )
  {
    return plugin;
  }

  public static PluginImageProvider getImageProvider( )
  {
    return getDefault().m_imgProvider;
  }

  /**
   * Returns an image descriptor for the image file at the given plug-in relative path.
   * 
   * @param path
   *          the path
   * @return the image descriptor
   */
  public static ImageDescriptor getImageDescriptor( String path )
  {
    return AbstractUIPlugin.imageDescriptorFromPlugin( "org.kalypso.model.wspm.tuhh.ui", path );
  }
}
