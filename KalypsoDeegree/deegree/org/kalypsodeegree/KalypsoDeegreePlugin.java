package org.kalypsodeegree;

import org.eclipse.core.runtime.Plugin;
import org.osgi.framework.BundleContext;

public class KalypsoDeegreePlugin extends Plugin
{
  // The shared instance.
  private static KalypsoDeegreePlugin m_plugin;

  public KalypsoDeegreePlugin( )
  {
    super();
    m_plugin = this;
  }

  /**
   * @see org.eclipse.core.runtime.Plugin#start(org.osgi.framework.BundleContext)
   */
  @Override
  public void start( BundleContext context ) throws Exception
  {
    super.start( context );
  }

  /**
   * @see org.eclipse.core.runtime.Plugin#stop(org.osgi.framework.BundleContext)
   */
  @Override
  public void stop( BundleContext context ) throws Exception
  {
    super.stop( context );
    m_plugin = null;
  }

  /**
   * Returns the shared instance.
   */
  public static KalypsoDeegreePlugin getDefault( )
  {
    return m_plugin;
  }

}
