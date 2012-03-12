package org.kalypso.ui.rrm.internal;

import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

public class KalypsoUIRRMPlugin extends AbstractUIPlugin
{
  private static KalypsoUIRRMPlugin PLUGIN;

  public KalypsoUIRRMPlugin( )
  {
    PLUGIN = this;
  }

  @Override
  public void start( final BundleContext context ) throws Exception
  {
    super.start( context );
  }

  @Override
  public void stop( final BundleContext context ) throws Exception
  {
    super.stop( context );
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

}
