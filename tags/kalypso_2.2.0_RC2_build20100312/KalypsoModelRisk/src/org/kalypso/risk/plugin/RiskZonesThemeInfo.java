package org.kalypso.risk.plugin;

import java.net.URL;
import java.util.Formatter;
import java.util.List;
import java.util.Properties;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.core.util.pool.PoolableObjectType;
import org.kalypso.core.util.pool.ResourcePool;
import org.kalypso.gml.ui.map.CoverageThemeInfo;
import org.kalypso.ogc.gml.IKalypsoThemeInfo;
import org.kalypso.risk.i18n.Messages;
import org.kalypso.risk.model.schema.binding.IRasterizationControlModel;
import org.kalypso.risk.model.schema.binding.IRiskZoneDefinition;
import org.kalypso.risk.preferences.KalypsoRiskPreferencePage;
import org.kalypso.risk.project.KalypsoRiskProjectNature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Position;

import de.renew.workflow.contexts.ICaseHandlingSourceProvider;

public class RiskZonesThemeInfo extends CoverageThemeInfo implements IKalypsoThemeInfo
{
  protected static final SortedMap<Double, String> m_riskZonesMap = new TreeMap<Double, String>();

  protected static boolean m_definitionsLoaded = false;

  private static String NATURE_PLANERCLIENT = "org.kalypso.planer.client.base.project.PlanerClientProjectNature"; //$NON-NLS-1$

  private static String CONTROLMODEL_NATURE_RISK = "/models/RasterizationControlModel.gml"; //$NON-NLS-1$

  private static String CONTROLMODEL_NATURE_PLANERCLIENT = "/results/risk/RasterizationControlModel.gml"; //$NON-NLS-1$

  public RiskZonesThemeInfo( )
  {
    if( !m_definitionsLoaded )
      loadDefinitions( true );
  }

  /**
   * @see org.kalypso.gml.ui.map.CoverageThemeInfo#initFormatString(java.util.Properties)
   */
  @Override
  protected String initFormatString( final Properties props )
  {
    final IPreferenceStore preferences = KalypsoRiskPreferencePage.getPreferences();
    final int digits = preferences.getInt( KalypsoRiskPreferencePage.KEY_RISKTHEMEINFO_IMPORTANTDIGITS );
    return props.getProperty( PROP_FORMAT, "%." + digits + "g \u20ac/m\u00b2/a - %s" ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.gml.ui.map.CoverageThemeInfo#appendQuickInfo(java.util.Formatter,
   *      org.kalypsodeegree.model.geometry.GM_Position)
   */
  @Override
  public void appendQuickInfo( final Formatter formatter, final GM_Position pos )
  {
    try
    {
      final Double value = getValue( pos );
      if( value == null )
        return;
      if( m_riskZonesMap.size() == 0 )
      {
        formatter.format( getDefaultFormatString(), value );
        return;
      }
      final Double key = value < 0.0 ? m_riskZonesMap.tailMap( value ).firstKey() : m_riskZonesMap.headMap( value ).lastKey();
      formatter.format( getFormatString(), Math.abs( value ), m_riskZonesMap.get( key ) );
    }
    catch( final Exception e )
    {
      formatter.format( Messages.getString( "org.kalypso.risk.plugin.RiskZonesThemeInfo.1" ), e.getLocalizedMessage() ); //$NON-NLS-1$
    }
  }

  public static final void reloadDefinitions( )
  {
    m_definitionsLoaded = false;
    loadDefinitions( false );
  }

  /**
   * High priority definition load is called directly from the map, i.e. definitions should be loaded ASAP. Reload calls
   * from the listeners (on model change) have less priority.
   */
  private static synchronized final void loadDefinitions( final boolean highPriority )
  {
    if( m_definitionsLoaded )
      return;
    final SortedMap<Double, String> map = RiskZonesThemeInfo.m_riskZonesMap;
    final Job job = new Job( "Risikozonen info laden" )
    {
      @Override
      protected IStatus run( final IProgressMonitor monitor )
      {
        final IFolder scenarioFolder = getScenarioFolder();
        if( scenarioFolder == null )
          return Status.OK_STATUS;
        try
        {
          final String resourcePath = getResourcePath( scenarioFolder );
          if( resourcePath == null )
            return Status.OK_STATUS;
          map.clear();

          final IFile riskControlModelFile = scenarioFolder.getFile( resourcePath );

          final URL databaseUrl = ResourceUtilities.createURL( riskControlModelFile );
          final PoolableObjectType poolKey = new PoolableObjectType( "gml", databaseUrl.toExternalForm(), databaseUrl ); //$NON-NLS-1$

          final ResourcePool pool = KalypsoCorePlugin.getDefault().getPool();
          final GMLWorkspace workspace = (GMLWorkspace) pool.getObject( poolKey );
          if( workspace == null )
            return Status.OK_STATUS;
          final IRasterizationControlModel model = (IRasterizationControlModel) workspace.getRootFeature().getAdapter( IRasterizationControlModel.class );
          final List<IRiskZoneDefinition> riskZonesList = model.getRiskZoneDefinitionsList();
          for( final IRiskZoneDefinition riskZoneDefinition : riskZonesList )
          {
            final Double key = new Double( riskZoneDefinition.getLowerBoundary() );
            if( riskZoneDefinition.isUrbanLanduseType() )
              map.put( key, riskZoneDefinition.getName() );
            else
              map.put( -key, riskZoneDefinition.getName() );
          }
        }
        catch( final Exception e )
        {
          Logger.getAnonymousLogger().log( Level.WARNING, e.getLocalizedMessage() );
        }
        finally
        {
          m_definitionsLoaded = true;
        }
        return Status.OK_STATUS;
      }
    };
    job.setUser( false );
    job.setRule( null );
    if( highPriority )
    {
      job.setPriority( Job.INTERACTIVE );
      job.schedule( 1 );
    }
    else
    {
      job.setPriority( Job.LONG );
      job.schedule( 500 );
    }
  }

  protected static final IFolder getScenarioFolder( )
  {
    final IWorkbench workbench = PlatformUI.getWorkbench();
    final IHandlerService handlerService = (IHandlerService) workbench.getService( IHandlerService.class );
    final IEvaluationContext context = handlerService.getCurrentState();
    final IFolder scenarioFolder = (IFolder) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_FOLDER_NAME );
    return scenarioFolder;
  }

  protected static final String getResourcePath( final IFolder scenarioFolder ) throws CoreException
  {
    if( scenarioFolder == null )
      return null;
    final IProject project = scenarioFolder.getProject();
    if( project.getNature( KalypsoRiskProjectNature.ID ) != null )
      return CONTROLMODEL_NATURE_RISK;
    else if( project.getNature( NATURE_PLANERCLIENT ) != null )
      return CONTROLMODEL_NATURE_PLANERCLIENT;
    else
      return null;
  }
}
