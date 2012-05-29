package org.kalypso.risk.plugin;

import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.IHandlerService;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.commons.eclipse.core.runtime.PluginImageProvider;
import org.kalypso.risk.preferences.KalypsoRiskPreferencePage;
import org.kalypso.risk.project.SzenarioController;
import org.osgi.framework.BundleContext;

import de.renew.workflow.connector.cases.IScenarioDataProvider;
import de.renew.workflow.contexts.ICaseHandlingSourceProvider;

/**
 * The activator class controls the plug-in life cycle
 */
public class KalypsoRiskPlugin extends AbstractUIPlugin
{
  public static final String PLUGIN_ID = "org.kalypso.risk"; //$NON-NLS-1$

  private static KalypsoRiskPlugin PLUGIN;

  private PluginImageProvider m_imageProvider;

  private SzenarioController m_szenarioController;

  public KalypsoRiskPlugin( )
  {
    super();
    PLUGIN = this;
  }

  public static KalypsoRiskPlugin getDefault( )
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

    // force plug-in to start
    // Dangerous: do not call this stuff in plugin-start method -> rather start a job to do it...
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

  public static final int getPreferences_themeInfoPrecision( )
  {
    final IPreferenceStore preferences = KalypsoRiskPlugin.getDefault().getPreferenceStore();
    int digits = preferences.getInt( KalypsoRiskPreferencePage.KEY_RISKTHEMEINFO_PRECISION );
    if( digits < KalypsoRiskPreferencePage.MIN_RISKTHEMEINFO_PRECISION || digits > KalypsoRiskPreferencePage.MAX_RISKTHEMEINFO_PRECISION )
      digits = KalypsoRiskPreferencePage.DEFAULT_RISKTHEMEINFO_PRECISION;
    return digits;
  }

  public SzenarioController getSzenarioController( )
  {
    return m_szenarioController;
  }

  protected void setSzenarioController( final SzenarioController szenarioController )
  {
    m_szenarioController = szenarioController;
  }

}
