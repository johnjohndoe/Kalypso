package org.kalypso.statistics.plugin;

import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.ImageRegistry;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.IHandlerService;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.commons.eclipse.core.runtime.PluginImageProvider;
import org.kalypso.statistics.project.SzenarioController;
import org.kalypso.statistics.types.EStatisticsImage;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;

import de.renew.workflow.connector.cases.IScenarioDataProvider;
import de.renew.workflow.contexts.ICaseHandlingSourceProvider;

/**
 * The activator class controls the plug-in life cycle
 */
public class KalypsoStatisticsPlugin extends AbstractUIPlugin
{
  // The plug-in ID
  public static final String PLUGIN_ID = "org.kalypso.statistics"; //$NON-NLS-1$

  private static KalypsoStatisticsPlugin PLUGIN;

  private PluginImageProvider m_imageProvider;

  private SzenarioController m_szenarioController;

  public KalypsoStatisticsPlugin( )
  {
    super();
    PLUGIN = this;
  }

  public static KalypsoStatisticsPlugin getDefault( )
  {
    return PLUGIN;
  }

  @Override
  public void start( final BundleContext context ) throws Exception
  {
    super.start( context );

    // delete tmp images both on startup and shutdown
    m_imageProvider = new PluginImageProvider( this );
    m_imageProvider.resetTmpFiles();

    final Job job = new Job( "" )
    {
      @Override
      protected IStatus run( final IProgressMonitor arg0 )
      {
        final IScenarioDataProvider dataProvider = KalypsoAFGUIFrameworkPlugin.getDefault().getDataProvider();
        setSzenarioController( new SzenarioController() );
        dataProvider.addScenarioDataListener( getSzenarioController() );
        getSzenarioController().scenarioChanged( dataProvider.getScenario() );
        return Status.OK_STATUS;
      }
    };
    job.setSystem( true );
    job.setUser( false );
    job.setPriority( Job.LONG );
    job.schedule();
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
      final IScenarioDataProvider caseDataProvider = (IScenarioDataProvider) currentState.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_DATA_PROVIDER_NAME );
      caseDataProvider.removeScenarioDataListener( getSzenarioController() );
    }

    PLUGIN = null;
    super.stop( context );
  }

  /**
   * @return the m_imageProvider
   */
  public static PluginImageProvider getImageProvider( )
  {
    return getDefault().m_imageProvider;
  }

  public SzenarioController getSzenarioController( )
  {
    return m_szenarioController;
  }

  protected void setSzenarioController( final SzenarioController szenarioController )
  {
    m_szenarioController = szenarioController;
  }

  @Override
  protected void initializeImageRegistry( final ImageRegistry registry )
  {
    super.initializeImageRegistry( registry );
    final Bundle bundle = Platform.getBundle( PLUGIN_ID );
    for( final EStatisticsImage eImage : EStatisticsImage.values() )
    {
      final ImageDescriptor imgDesc = ImageDescriptor.createFromURL( FileLocator.find( bundle, new Path( eImage.getImagePath() ), null ) );
      registry.put( eImage.name(), imgDesc );
    }
  }
}
