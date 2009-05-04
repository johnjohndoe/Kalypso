package org.kalypso.risk.model.handlers;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IHandler;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Status;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.gml.ui.map.CoverageManagementWidget;
import org.kalypso.ogc.gml.AbstractCascadingLayerTheme;
import org.kalypso.ogc.gml.CascadingThemeHelper;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.widgets.ActivateWidgetJob;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.mapmodel.MapModellHelper;
import org.kalypso.risk.i18n.Messages;
import org.kalypso.ui.views.map.MapView;

public class WaterdepthCoveragesWidgetHandler extends AbstractHandler implements IHandler
{

  /**
   * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
   */
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
      throw new ExecutionException( Messages.getString( "org.kalypso.risk.model.handlers.WaterdepthCoveragesWidgetHandler.0" ) ); //$NON-NLS-1$
    }

    final IMapPanel mapPanel = mapView.getMapPanel();

    /* wait for map to load */
    if( !MapModellHelper.waitForAndErrorDialog( shell, mapPanel, "org.kalypso.risk.model.handlers.WaterdepthCoveragesWidgetHandler.3", "org.kalypso.risk.model.handlers.WaterdepthCoveragesWidgetHandler.5" ) )  //$NON-NLS-1$ //$NON-NLS-2$
    {
      return null;
    }

    final IMapModell mapModell = mapPanel.getMapModell();
    if( mapModell != null )
    {
      // get "Wasserspiegellagen" cascading theme
      final AbstractCascadingLayerTheme hqTheme = CascadingThemeHelper.getNamedCascadingTheme( mapModell, "HQi" ); //$NON-NLS-1$
      if( hqTheme != null )
      {
        mapModell.activateTheme( hqTheme );
      }
    }

    try
    {
      final CoverageManagementWidget widget = new CoverageManagementWidget( Messages.getString( "org.kalypso.risk.model.handlers.WaterdepthCoveragesWidgetHandler.4" ), "" ); //$NON-NLS-1$ //$NON-NLS-2$
      final IFolder scenarioFolder = KalypsoAFGUIFrameworkPlugin.getDefault().getActiveWorkContext().getCurrentCase().getFolder();
      widget.setGridFolder( scenarioFolder.getFolder( "grids" ) ); //$NON-NLS-1$
      widget.setAllowUserChangeGridFolder( false );

      final ActivateWidgetJob job = new ActivateWidgetJob( "Select Widget", widget, mapPanel, activePage ); //$NON-NLS-1$
      job.schedule();
    }
    catch( final CoreException e )
    {
      throw new ExecutionException( org.kalypso.risk.i18n.Messages.getString("org.kalypso.risk.model.handlers.WaterdepthCoveragesWidgetHandler.6"), e ); //$NON-NLS-1$
    }

    return Status.OK_STATUS;
  }

}
