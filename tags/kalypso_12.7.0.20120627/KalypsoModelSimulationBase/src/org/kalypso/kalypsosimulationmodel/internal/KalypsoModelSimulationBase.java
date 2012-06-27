package org.kalypso.kalypsosimulationmodel.internal;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.plugin.AbstractUIPlugin;

public class KalypsoModelSimulationBase extends AbstractUIPlugin
{
  public static String PLUGIN_ID = "org.kalypso.ModelSimulationBase"; //$NON-NLS-1$

  // The shared instance
  private static KalypsoModelSimulationBase plugin;

  public KalypsoModelSimulationBase( )
  {
    plugin = this;
  }

  /**
   * Returns the shared instance
   * 
   * @return the shared instance
   */
  public static KalypsoModelSimulationBase getDefault( )
  {
    return plugin;
  }

  /**
   * Returns an image descriptor for the image file at the given plug-in relative path
   * 
   * @param path
   *          the path
   * @return the image descriptor
   */
  public static ImageDescriptor getImageDescriptor( final String path )
  {
    return imageDescriptorFromPlugin( PLUGIN_ID, path );
  }
}