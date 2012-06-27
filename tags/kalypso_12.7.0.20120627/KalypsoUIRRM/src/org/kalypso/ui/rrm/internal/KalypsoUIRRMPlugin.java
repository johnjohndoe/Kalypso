package org.kalypso.ui.rrm.internal;

import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.kalypso.commons.eclipse.core.runtime.PluginImageProvider;
import org.osgi.framework.BundleContext;

public class KalypsoUIRRMPlugin extends AbstractUIPlugin
{
  private static KalypsoUIRRMPlugin PLUGIN;

  private PluginImageProvider m_imageProvider;

  public KalypsoUIRRMPlugin( )
  {
    PLUGIN = this;
  }

  @Override
  public void start( final BundleContext context ) throws Exception
  {
    super.start( context );

    m_imageProvider = new PluginImageProvider( this );
    m_imageProvider.resetTmpFiles();
  }

  @Override
  public void stop( final BundleContext context ) throws Exception
  {
    super.stop( context );

    /* Delete temporary images shutdown. */
    m_imageProvider.resetTmpFiles();
    m_imageProvider = null;

    PLUGIN = null;
  }

  public static KalypsoUIRRMPlugin getDefault( )
  {
    return PLUGIN;
  }

  public static String getID( )
  {
    return getDefault().getBundle().getSymbolicName();
  }

  public PluginImageProvider getImageProvider( )
  {
    return m_imageProvider;
  }

}
