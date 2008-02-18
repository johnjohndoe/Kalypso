package org.kalypso.model.flood.handlers;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IHandler;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFolder;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.kalypso.afgui.scenarios.ScenarioHelper;
import org.kalypso.gml.ui.map.CoverageManagementWidget;
import org.kalypso.model.flood.util.FloodModelHelper;
import org.kalypso.ogc.gml.AbstractCascadingLayerTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.widgets.ActivateWidgetJob;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.mapmodel.MapModellHelper;
import org.kalypso.ui.views.map.MapView;

public class VisualizeDepthDataHandler extends AbstractHandler implements IHandler
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
      throw new ExecutionException( "Kartenansicht nicht geöffnet." );

    final MapPanel mapPanel = mapView.getMapPanel();

    /* wait for map to load */
    if( !MapModellHelper.waitForAndErrorDialog( shell, mapPanel, "WSP-Anpassen", "Fehler beim Öffnen der Karte" ) )
      return null;

    final IMapModell mapModell = mapPanel.getMapModell();
    if( mapModell != null )
    {
      // get "Wasserspiegellagen" cascading theme
      final AbstractCascadingLayerTheme wspTheme = FloodModelHelper.findWspTheme( mapModell );
      if( wspTheme != null )
        mapModell.activateTheme( wspTheme );
    }

    final CoverageManagementWidget coverageManagementWidget = new CoverageManagementWidget( "Ergebnisse verwalten", "Ergebnisse verwalten" );
    final IFolder scenarioFolder = ScenarioHelper.getScenarioFolder();
    coverageManagementWidget.setGridFolder( scenarioFolder.getFolder( "grids" ) );

    final IWorkbenchPart activePart = (IWorkbenchPart) context.getVariable( ISources.ACTIVE_PART_NAME );
    final Display display = shell.isDisposed() ? activePart.getSite().getShell().getDisplay() : shell.getDisplay();

    final ActivateWidgetJob job = new ActivateWidgetJob( display, "Select Widget", coverageManagementWidget, mapPanel, activePart );
    job.schedule();

    return null;
  }

}
