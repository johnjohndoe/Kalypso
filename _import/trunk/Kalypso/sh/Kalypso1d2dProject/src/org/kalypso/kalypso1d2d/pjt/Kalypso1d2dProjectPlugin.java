package org.kalypso.kalypso1d2d.pjt;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.URL;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.ImageRegistry;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
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

  private static ActiveWorkContext m_activeWorkContext;

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
    final IWorkbench workbench = PlatformUI.getWorkbench();
    m_activeWorkContext = new ActiveWorkContext();
    workbench.addWindowListener( m_activeWorkContext );
  }

  /**
   * @see org.eclipse.ui.plugin.AbstractUIPlugin#stop(org.osgi.framework.BundleContext)
   */
  @Override
  public void stop( BundleContext context ) throws Exception
  {
    plugin = null;
    // TODO: dispose activeWorkContext
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

  public void showMessage( final String message )
  {
    plugin.getWorkbench().getDisplay().syncExec( new Runnable()
    {
      public void run( )
      {
        Shell shell = plugin.getWorkbench().getDisplay().getActiveShell();
        MessageDialog.openInformation( shell, "Message", message );
      }
    } );
  }

  public boolean askQuestion( final String message )
  {

    Shell shell = plugin.getWorkbench().getDisplay().getActiveShell();
    return MessageDialog.openConfirm( shell, "Message", message );

  }

  public void showException( String message, Throwable th )
  {
    StringWriter sw = new StringWriter();
    PrintWriter pw = new PrintWriter( sw );
    pw.print( message );
    pw.print( "\n=======================================\n" );
    th.printStackTrace( pw );
    this.showMessage( sw.getBuffer().toString() );
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

  public static ActiveWorkContext getActiveWorkContext( )
  {
    return m_activeWorkContext;
  }
}
