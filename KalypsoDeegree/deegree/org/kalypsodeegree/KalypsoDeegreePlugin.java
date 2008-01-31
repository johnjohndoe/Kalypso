package org.kalypsodeegree;

import java.io.File;
import java.util.logging.Logger;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Plugin;
import org.kalypsodeegree_impl.graphics.sld.DefaultStyleFactory;
import org.osgi.framework.BundleContext;

public class KalypsoDeegreePlugin extends Plugin
{
  private static final Logger LOGGER = Logger.getLogger( KalypsoDeegreePlugin.class.getName() );

  // The shared instance.
  private static KalypsoDeegreePlugin m_plugin;

  private DefaultStyleFactory m_defaultStyleFactory;

  public KalypsoDeegreePlugin( )
  {
    super();
    m_plugin = this;
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
    super.stop( context );
    m_plugin = null;
    m_defaultStyleFactory = null;
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
}
