package org.kalypso.risk.model.handlers;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IHandler;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.kalypso.gml.ui.map.CoverageManagementWidget;
import org.kalypso.ogc.gml.AbstractCascadingLayerTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.widgets.ActivateWidgetJob;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.mapmodel.MapModellHelper;
import org.kalypso.risk.model.actions.dataImport.waterdepth.ImportWaterdepthWizard;
import org.kalypso.ui.views.map.MapView;

public class WaterdepthCoveragesWidgetHandler extends AbstractHandler implements IHandler
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
      throw new ExecutionException( "Kartenansicht nicht ge�ffnet." );

    final MapPanel mapPanel = mapView.getMapPanel();

    /* wait for map to load */
    if( !MapModellHelper.waitForAndErrorDialog( shell, mapPanel, "WSP-Anpassen", "Fehler beim öffnen der Karte" ) )
      return null;

    final IMapModell mapModell = mapPanel.getMapModell();
    if( mapModell != null )
    {
      // get "Wasserspiegellagen" cascading theme
      final AbstractCascadingLayerTheme hqTheme = ImportWaterdepthWizard.getWaterdepthsCascadingTheme( mapModell );
      if( hqTheme != null )
        mapModell.activateTheme( hqTheme );
    }

    final CoverageManagementWidget coverageManagementWidget = new CoverageManagementWidget("Waterlevel data editing", "");
//    coverageManagementWidget.setShowStyle( false );
//    coverageManagementWidget.setShowAddRemoveButtons( false );

    final IWorkbenchPart activePart = (IWorkbenchPart) context.getVariable( ISources.ACTIVE_PART_NAME );
    final Display display = shell.isDisposed() ? activePart.getSite().getShell().getDisplay() : shell.getDisplay();

    ActivateWidgetJob job = new ActivateWidgetJob( display, "Select Widget", coverageManagementWidget, mapPanel, activePart );
    job.schedule();

    return null;
  }

}
