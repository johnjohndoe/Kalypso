package org.kalypso.risk.plugin;

import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;
import org.eclipse.ui.handlers.IHandlerService;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.commons.eclipse.core.runtime.PluginImageProvider;
import org.kalypso.risk.preferences.KalypsoRiskPreferencePage;
import org.kalypso.risk.project.SzenarioController;
import org.osgi.framework.BundleContext;

import de.renew.workflow.connector.worklist.TaskExecutionAuthority;
import de.renew.workflow.connector.worklist.TaskExecutionListener;
import de.renew.workflow.contexts.ICaseHandlingSourceProvider;

/**
 * The activator class controls the plug-in life cycle
 */
public class KalypsoRiskPlugin extends AbstractUIPlugin
{
  private static KalypsoRiskPlugin PLUGIN;

  private TaskExecutionAuthority m_taskExecutionAuthority;

  private TaskExecutionListener m_taskExecutionListener;

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

    if( PlatformUI.isWorkbenchRunning() )
    {
      final IWorkbench workbench = PlatformUI.getWorkbench();
      // TODO: check if this stuff is really necessary! This is copy paste from AFGUI stuff, probably not needed twice

      final ICommandService commandService = (ICommandService) workbench.getService( ICommandService.class );
      m_taskExecutionListener = new TaskExecutionListener();
      commandService.addExecutionListener( m_taskExecutionListener );
      m_taskExecutionAuthority = new TaskExecutionAuthority();
    }

    // delete tmp images both on startup and shutdown
    m_imageProvider = new PluginImageProvider( this );
    m_imageProvider.resetTmpFiles();

    // force plug-in to start
    final SzenarioDataProvider dataProvider = KalypsoAFGUIFrameworkPlugin.getDefault().getDataProvider();
    m_szenarioController = new SzenarioController();
    dataProvider.addScenarioDataListener( m_szenarioController );
    m_szenarioController.scenarioChanged( dataProvider.getScenario() );
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

    PLUGIN = null;
    super.stop( context );
  }

  public TaskExecutionAuthority getTaskExecutionAuthority( )
  {
    return m_taskExecutionAuthority;
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

}
