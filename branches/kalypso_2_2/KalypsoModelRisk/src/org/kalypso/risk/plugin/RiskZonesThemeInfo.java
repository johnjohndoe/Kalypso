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
import org.eclipse.core.resources.ResourcesPlugin;
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
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Position;

import de.renew.workflow.contexts.ICaseHandlingSourceProvider;

public class RiskZonesThemeInfo extends CoverageThemeInfo implements IKalypsoThemeInfo
{
  private final SortedMap<Double, String> m_riskZonesMap = new TreeMap<Double, String>();

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
      loadDefinitions();
      if( m_riskZonesMap.size() == 0 )
      {
        formatter.format( getDefaultFormatString(), value );
        return;
      }
      final Double key = value < 0.0 ? m_riskZonesMap.tailMap( value ).firstKey() : m_riskZonesMap.headMap( value ).lastKey();
      formatter.format( getFormatString(), Math.abs( value ), m_riskZonesMap.get( key ) );
    }
    catch( Exception e )
    {
      e.printStackTrace();
      formatter.format( Messages.getString( "org.kalypso.risk.plugin.RiskZonesThemeInfo.1" ), e.toString() ); //$NON-NLS-1$
    }
  }

  /**
   * Risk user have the possibility to rename the risk zones (boundaries, descriptions). On landuse import, risk zone
   * definitions are updated as well. If that happen, the update should be reflected here.
   * 
   */
  private void loadDefinitions( )
  {
    final IWorkbench workbench = PlatformUI.getWorkbench();
    final IHandlerService handlerService = (IHandlerService) workbench.getService( IHandlerService.class );
    final IEvaluationContext context = handlerService.getCurrentState();
    final IFolder scenarioFolder = (IFolder) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_FOLDER_NAME );
    // if no scenario is active, just return
    if( scenarioFolder == null )
      return;

    m_riskZonesMap.clear();
    final IProject project = scenarioFolder.getProject();
    try
    {

      // //////////////////////////////////////////

      final String resourcePath;
      if( project.getNature( "org.kalypso.risk.project.KalypsoRiskProjectNature" ) != null ) { //$NON-NLS-1$
        resourcePath = "/models/RasterizationControlModel.gml"; //$NON-NLS-1$
      }
      else if( project.getNature( "org.kalypso.planer.client.base.project.PlanerClientProjectNature" ) != null ) { //$NON-NLS-1$
        resourcePath = "/results/risk/RasterizationControlModel.gml"; //$NON-NLS-1$
      }
      else
        return;

      // //////////////////////////////////////////

      final IFile riskControlModelFile = scenarioFolder.getFile( resourcePath );

      final URL databaseUrl = ResourceUtilities.createURL( riskControlModelFile );
      final PoolableObjectType poolKey = new PoolableObjectType( "gml", databaseUrl.toExternalForm(), databaseUrl ); //$NON-NLS-1$

      final ResourcePool pool = KalypsoCorePlugin.getDefault().getPool();
      final GMLWorkspace workspace = (GMLWorkspace) pool.getObject( poolKey );
      if( workspace == null )
        return;
      final IRasterizationControlModel model = (IRasterizationControlModel) workspace.getRootFeature().getAdapter( IRasterizationControlModel.class );
      final List<IRiskZoneDefinition> riskZonesList = model.getRiskZoneDefinitionsList();
      for( final IRiskZoneDefinition riskZoneDefinition : riskZonesList )
      {
        final Double key = new Double( riskZoneDefinition.getLowerBoundary() );
        if( riskZoneDefinition.isUrbanLanduseType() )
          m_riskZonesMap.put( key, riskZoneDefinition.getName() );
        else
          m_riskZonesMap.put( -key, riskZoneDefinition.getName() );
      }
    }
    catch( final Exception e )
    {
      Logger.getAnonymousLogger().log( Level.WARNING, e.getLocalizedMessage() );
    }
  }

}
