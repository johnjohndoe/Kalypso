package org.kalypso.risk.model.handlers;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.widgets.ActivateWidgetJob;
import org.kalypso.ogc.gml.mapmodel.MapModellHelper;
import org.kalypso.risk.i18n.Messages;
import org.kalypso.risk.model.actions.manageWaterdepthCollections.WaterdepthCollectionsManagementWidget;
import org.kalypso.risk.model.utils.RiskModelHelper;
import org.kalypso.ui.views.map.MapView;

public class WaterdepthCollectionsWidgetHandler extends AbstractHandler
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
      throw new ExecutionException( Messages.getString( "org.kalypso.risk.model.handlers.WaterdepthCollectionsWidgetHandler.0" ) ); //$NON-NLS-1$

    final IMapPanel mapPanel = mapView.getMapPanel();

    /* wait for map to load */
    if( !MapModellHelper.waitForAndErrorDialog( shell, mapPanel, Messages.getString( "org.kalypso.risk.model.handlers.WaterdepthCollectionsWidgetHandler.1" ), Messages.getString( "org.kalypso.risk.model.handlers.WaterdepthCollectionsWidgetHandler.2" ) ) ) //$NON-NLS-1$ //$NON-NLS-2$
      return null;

    RiskModelHelper.activateEventTheme( mapPanel );

    final WaterdepthCollectionsManagementWidget widget = new WaterdepthCollectionsManagementWidget();

    final ActivateWidgetJob job = new ActivateWidgetJob( "Select Widget", widget, mapPanel, activePage ); //$NON-NLS-1$
    job.schedule();

    return null;
  }
}