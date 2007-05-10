package org.kalypso.kalypso1d2d.pjt;

import java.net.URL;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.ImageRegistry;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.IHandlerService;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.kalypso.afgui.scenarios.Scenario;
import org.kalypso.kalypso1d2d.pjt.actions.PerspectiveWatcher;
import org.osgi.framework.BundleContext;

import de.renew.workflow.base.ISzenarioSourceProvider;
import de.renew.workflow.cases.ICaseDataProvider;
import de.renew.workflow.connector.WorkflowConnectorPlugin;
import de.renew.workflow.connector.context.ActiveWorkContext;

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

  private final PerspectiveWatcher<Scenario> m_perspectiveWatcher = new PerspectiveWatcher<Scenario>();

  private SzenarioSourceProvider m_szenarioSourceProvider;

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
    final ActiveWorkContext<Scenario> activeWorkContext = WorkflowConnectorPlugin.getDefault().getActiveWorkContext();
    final IWorkbench workbench = PlatformUI.getWorkbench();
    final IHandlerService handlerService = (IHandlerService) workbench.getService( IHandlerService.class );
    m_szenarioSourceProvider = new SzenarioSourceProvider( activeWorkContext );
    handlerService.addSourceProvider( m_szenarioSourceProvider );
    activeWorkContext.addActiveContextChangeListener( m_perspectiveWatcher );
  }

  /**
   * @see org.eclipse.ui.plugin.AbstractUIPlugin#stop(org.osgi.framework.BundleContext)
   */
  @Override
  public void stop( BundleContext context ) throws Exception
  {
    plugin = null;

    final IWorkbench workbench = PlatformUI.getWorkbench();
    final IHandlerService handlerService = (IHandlerService) workbench.getService( IHandlerService.class );
    handlerService.removeSourceProvider( m_szenarioSourceProvider );
    final ActiveWorkContext<Scenario> activeWorkContext = WorkflowConnectorPlugin.getDefault().getActiveWorkContext();
    activeWorkContext.removeActiveContextChangeListener( m_perspectiveWatcher );
    activeWorkContext.setActiveProject( null );
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

  public ICaseDataProvider getDataProvider( )
  {
    return (ICaseDataProvider) m_szenarioSourceProvider.getCurrentState().get( ISzenarioSourceProvider.ACTIVE_SZENARIO_DATA_PROVIDER_NAME );
  }
}
