package org.kalypso.portal;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

/**
 * The main plugin class to be used in the desktop.
 */
public class KalypsoPortalPlugin extends AbstractUIPlugin
{

  // The shared instance.
  private static KalypsoPortalPlugin plugin;
  private static String m_id = "org.kalypso.portal";

//  private URL m_context = null;

  /**
   * The constructor.
   */
  public KalypsoPortalPlugin( )
  {
    plugin = this;
  }

  /**
   * This method is called upon plug-in activation
   */
  @Override
  public void start( BundleContext context ) throws Exception
  {
    super.start( context );
  }

  /**
   * This method is called when the plug-in is stopped
   */
  @Override
  public void stop( BundleContext context ) throws Exception
  {
    super.stop( context );
    plugin = null;
  }

  /**
   * Returns the shared instance.
   */
  public static KalypsoPortalPlugin getDefault( )
  {
    return plugin;
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
    return AbstractUIPlugin.imageDescriptorFromPlugin( "KalypsoPortal", path );
  }

  public static String getID( )
  {
    return m_id;
  }

  // public void setContext( final IProject project )
  // {
  // try
  // {
  // m_context = ResourceUtilities.createURL( project );
  // }
  // catch( MalformedURLException e )
  // {
  // e.printStackTrace();
  // }
  // }
  //
  // public URL getContext( )
  // {
  // return m_context;
  // }
}
