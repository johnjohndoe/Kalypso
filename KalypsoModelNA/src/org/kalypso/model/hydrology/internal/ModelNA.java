package org.kalypso.model.hydrology.internal;

import org.eclipse.core.runtime.Plugin;
import org.osgi.framework.BundleContext;

public class ModelNA extends Plugin
{
  public static final String PLUGIN_ID = "org.kalypso.NACalcJob"; // NO_UCD //$NON-NLS-1$

  private static ModelNA plugin;

  @Override
  public void start( final BundleContext context ) throws Exception
  {
    super.start( context );
    plugin = this;
  }

  @Override
  public void stop( final BundleContext context ) throws Exception
  {
    plugin = null;
    super.stop( context );
  }

  /**
   * Returns the shared instance
   * 
   * @return the shared instance
   */
  public static ModelNA getDefault( )
  {
    return plugin;
  }

}
