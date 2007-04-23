package org.kalypso.kalypso1d2d.pjt;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.net.URL;
import java.util.Properties;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.ImageRegistry;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

/**
 * The activator class controls the plug-in life cycle
 */
public class Kalypso1d2dProjectPlugin extends AbstractUIPlugin
{

  // The plug-in ID
  public static final String PLUGIN_ID = "org.eclipse.kalypso1d2d.pjt.Kalypso1d2dProject";

  // The shared instance
  static Kalypso1d2dProjectPlugin plugin;

  private static final String ICON_SIM_MODEL_PATH = "/icons/nuvola_select/ledblue.png";

  public static final String KEY_ICON_SIM_MODEL = "_ICON_SIM_MODEL_";

  private static final String ACTIVE_WORKCONTEXT_MEMENTO = "activeWorkContext";

  private ActiveWorkContext m_activeWorkContext;

  /**
   * The constructor
   */
  public Kalypso1d2dProjectPlugin( )
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
    Properties properties = new Properties();
    final String fileName = getStateLocation().append( ACTIVE_WORKCONTEXT_MEMENTO ).toOSString();
    final File file = new File( fileName );
    if( file.exists() )
    {
      properties.loadFromXML( new FileInputStream( file ) );
    }
    m_activeWorkContext = new ActiveWorkContext( properties );
  }

  /**
   * @see org.eclipse.ui.plugin.AbstractUIPlugin#stop(org.osgi.framework.BundleContext)
   */
  @Override
  public void stop( BundleContext context ) throws Exception
  {
    plugin = null;
    final Properties properties = m_activeWorkContext.createProperties();
    final String fileName = getStateLocation().append( ACTIVE_WORKCONTEXT_MEMENTO ).toOSString();
    final File file = new File( fileName );
    if( file.exists() )
    {
      file.delete();
    }
    properties.storeToXML( new FileOutputStream( file ), "" );
    m_activeWorkContext.setActiveProject( null );
    m_activeWorkContext.dispose();
    m_activeWorkContext = null;
    super.stop( context );
  }

  /**
   * Returns the shared instance
   * 
   * @return the shared instance
   */
  public static Kalypso1d2dProjectPlugin getDefault( )
  {
    return plugin;
  }

  @Override
  protected void initializeImageRegistry( ImageRegistry reg )
  {
    String couples[][] = { { KEY_ICON_SIM_MODEL, ICON_SIM_MODEL_PATH } };

    // TODO dipose images on stop
    for( String[] curCouple : couples )
    {
      URL url = getBundle().getEntry( curCouple[1] );
      ImageDescriptor desc = ImageDescriptor.createFromURL( url );
      // reg.put(curCouple[0], desc);
      reg.put( KEY_ICON_SIM_MODEL, desc.createImage() );
    }
    return;
  }  

  public static Image getImageDescriptor( String key )
  {
    return getDefault().getImageRegistry().get( key );
  }

  public ActiveWorkContext getActiveWorkContext( )
  {
    return m_activeWorkContext;
  }
}
