package org.kalypso.kalypso1d2d.pjt;

import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.IHandlerService;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.afgui.scenarios.IScenario;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.commons.eclipse.core.runtime.PluginImageProvider;
import org.kalypso.kalypso1d2d.pjt.i18n.Messages;
import org.kalypso.kalypso1d2d.pjt.views.SzenarioController;
import org.osgi.framework.BundleContext;

import de.renew.workflow.contexts.ICaseHandlingSourceProvider;

/**
 * The activator class controls the plug-in life cycle
 */
public class Kalypso1d2dProjectPlugin extends AbstractUIPlugin
{

  // The plug-in ID
  public static final String PLUGIN_ID = "org.eclipse.kalypso1d2d.pjt.Kalypso1d2dProject"; //$NON-NLS-1$

  // The shared instance
  private static Kalypso1d2dProjectPlugin plugin;

  private SzenarioController m_szenarioController;

  private PluginImageProvider m_imageProvider;

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
  public void start( final BundleContext context ) throws Exception
  {
    super.start( context );

    // delete tmp images both on startup and shutdown
    m_imageProvider = new PluginImageProvider( this );
    m_imageProvider.resetTmpFiles();

    // Initialize this controller inside a job, else this plugin will not get loaded
    // if anything happens (was the case on some machines, probably due to race conditions)
    final Job job = new Job( Messages.getString("org.kalypso.kalypso1d2d.pjt.Kalypso1d2dProjectPlugin.0") ) //$NON-NLS-1$
    {
      @Override
      protected IStatus run( IProgressMonitor monitor )
      {
        initScenarioController();
        
        return Status.OK_STATUS;
      }
    };
    job.setSystem( true );
    job.schedule( 2000 );
  }

  /**
   * @see org.eclipse.ui.plugin.AbstractUIPlugin#stop(org.osgi.framework.BundleContext)
   */
  @Override
  public void stop( final BundleContext context ) throws Exception
  {
    // delete tmp images both on startup and shutdown
    m_imageProvider.resetTmpFiles();
    m_imageProvider = null;

    if( PlatformUI.isWorkbenchRunning() )
    {
      final IWorkbench workbench = PlatformUI.getWorkbench();
      final IHandlerService service = (IHandlerService) workbench.getService( IHandlerService.class );
      final IEvaluationContext currentState = service.getCurrentState();
      final SzenarioDataProvider caseDataProvider = (SzenarioDataProvider) currentState.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_DATA_PROVIDER_NAME );
      caseDataProvider.removeScenarioDataListener( m_szenarioController );
    }
    plugin = null;
    super.stop( context );
  }

  protected void initScenarioController( )
  {
    final SzenarioDataProvider dataProvider = KalypsoAFGUIFrameworkPlugin.getDefault().getDataProvider();
    m_szenarioController = new SzenarioController();
    dataProvider.addScenarioDataListener( m_szenarioController );
    final IScenario scenario = dataProvider.getScenario();
    m_szenarioController.scenarioChanged( scenario );
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

  public static ImageDescriptor getImageDescriptor( final String path )
  {
    return imageDescriptorFromPlugin( PLUGIN_ID, path );
  }

  public static PluginImageProvider getImageProvider( )
  {
    return getDefault().m_imageProvider;
  }
}
