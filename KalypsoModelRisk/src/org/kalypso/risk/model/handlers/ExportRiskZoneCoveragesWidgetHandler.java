package org.kalypso.risk.model.handlers;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IHandler;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFolder;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.kalypso.afgui.scenarios.ScenarioHelper;
import org.kalypso.gml.ui.map.CoverageManagementWidget;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.widgets.ActivateWidgetJob;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.mapmodel.MapModellHelper;
import org.kalypso.risk.Messages;
import org.kalypso.ui.views.map.MapView;

public class ExportRiskZoneCoveragesWidgetHandler extends AbstractHandler implements IHandler
{

  /**
   * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
   */
  @Override
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    /* Get context */
    final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();
    final Shell shell = (Shell) context.getVariable( ISources.ACTIVE_SHELL_NAME );

    /* Get the map */
    final IWorkbenchWindow window = (IWorkbenchWindow) context.getVariable( ISources.ACTIVE_WORKBENCH_WINDOW_NAME );
    final MapView mapView = (MapView) window.getActivePage().findView( MapView.ID );
    if( mapView == null )
      throw new ExecutionException( Messages.getString( "ExportRiskZoneCoveragesWidgetHandler.0" ) ); //$NON-NLS-1$

    final MapPanel mapPanel = mapView.getMapPanel();

    /* wait for map to load */
    if( !MapModellHelper.waitForAndErrorDialog( shell, mapPanel, Messages.getString( "ExportRiskZoneCoveragesWidgetHandler.1" ), Messages.getString( "ExportRiskZoneCoveragesWidgetHandler.2" ) ) ) //$NON-NLS-1$ //$NON-NLS-2$
      return null;

    final IMapModell mapModell = mapPanel.getMapModell();
    if( mapModell != null )
    {
      final IKalypsoTheme[] themes = mapModell.getAllThemes();
      for( final IKalypsoTheme element : themes )
      {
        final String themeProperty = element.getProperty( "themeId", "" ); //$NON-NLS-1$ //$NON-NLS-2$
        // check below is because of downgrade purposes
        if( themeProperty.equals( "riskZonesGridTheme" ) || element.getName().getKey() == "Risikozonen (gerastert)" || element.getName().getKey() == "Risikozonen (raster)" ) //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        {
          mapModell.activateTheme( element );
          break;
        }
      }
    }

    final CoverageManagementWidget coverageManagementWidget = new CoverageManagementWidget( Messages.getString( "ExportRiskZoneCoveragesWidgetHandler.3" ), "" ); //$NON-NLS-1$ //$NON-NLS-2$
    coverageManagementWidget.setShowStyle( false );
    coverageManagementWidget.setShowAddRemoveButtons( false );
    final IFolder scenarioFolder = ScenarioHelper.getScenarioFolder();
    coverageManagementWidget.setGridFolder( scenarioFolder.getFolder( "grids" ) ); //$NON-NLS-1$

    final IWorkbenchPart activePart = (IWorkbenchPart) context.getVariable( ISources.ACTIVE_PART_NAME );

    final ActivateWidgetJob job = new ActivateWidgetJob( "Select Widget", coverageManagementWidget, mapPanel, activePart ); //$NON-NLS-1$
    job.schedule();

    return null;
  }
}
