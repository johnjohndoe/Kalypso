package org.kalypso.model.flood.handlers;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFolder;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.gml.ui.coverage.CoverageManagementWidget;
import org.kalypso.model.flood.i18n.Messages;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.widgets.ActivateWidgetJob;
import org.kalypso.ogc.gml.mapmodel.MapModellHelper;
import org.kalypso.ui.views.map.MapView;

public class VisualizeGridDataHandler extends AbstractHandler
{
  @Override
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    /* Get context */
    final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();
    final Shell shell = (Shell) context.getVariable( ISources.ACTIVE_SHELL_NAME );

    /* Get the map */
    final IWorkbenchWindow window = (IWorkbenchWindow) context.getVariable( ISources.ACTIVE_WORKBENCH_WINDOW_NAME );
    final IWorkbenchPage activePage = window.getActivePage();
    final MapView mapView = (MapView) activePage.findView( MapView.ID );
    if( mapView == null )
    {
      throw new ExecutionException( Messages.getString("org.kalypso.model.flood.handlers.VisualizeGridDataHandler.0") ); //$NON-NLS-1$
    }

    final IMapPanel mapPanel = mapView.getMapPanel();

    /* wait for map to load */
    if( !MapModellHelper.waitForAndErrorDialog( shell, mapPanel, Messages.getString("org.kalypso.model.flood.handlers.VisualizeGridDataHandler.1"), Messages.getString("org.kalypso.model.flood.handlers.VisualizeGridDataHandler.2") ) ) //$NON-NLS-1$ //$NON-NLS-2$
    {
      return null;
    }

    final CoverageManagementWidget coverageManagementWidget = new CoverageManagementWidget( Messages.getString( "org.kalypso.model.flood.handlers.VisualizeGridDataHandler.3" ), Messages.getString( "org.kalypso.model.flood.handlers.VisualizeGridDataHandler.4" ) ); //$NON-NLS-1$ //$NON-NLS-2$
    final IFolder scenarioFolder = KalypsoAFGUIFrameworkPlugin.getActiveWorkContext().getCurrentCase().getFolder();
    coverageManagementWidget.setGridFolder( scenarioFolder.getFolder( "grids" ) ); //$NON-NLS-1$
    coverageManagementWidget.setAllowUserChangeGridFolder( false );

    final ActivateWidgetJob job = new ActivateWidgetJob( Messages.getString( "org.kalypso.model.flood.handlers.VisualizeGridDataHandler.6" ), coverageManagementWidget, mapPanel, activePage ); //$NON-NLS-1$
    job.schedule();

    return null;
  }
}