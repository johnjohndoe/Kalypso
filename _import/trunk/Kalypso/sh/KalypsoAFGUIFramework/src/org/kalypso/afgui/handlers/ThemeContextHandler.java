/**
 *
 */
package org.kalypso.afgui.handlers;

import java.util.Properties;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.Status;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.mapmodel.ActivateThemeJob;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.mapmodel.MapModellHelper;
import org.kalypso.ui.views.map.MapView;

/**
 * Activates a given theme in the current map view.
 * 
 * @author Stefan Kurzbach
 */
public class ThemeContextHandler extends AbstractHandler
{
  private final String m_featureType;

  private ActivateThemeJob m_activateThemeJob;

  public ThemeContextHandler( final Properties properties )
  {
    m_featureType = properties.getProperty( KalypsoContextHandlerFactory.PARAM_INPUT );

    Assert.isNotNull( m_featureType, "Parameter 'input' not set for themeContext" );
  }

  /**
   * @see org.eclipse.core.commands.AbstractHandler#dispose()
   */
  @Override
  public void dispose( )
  {
    if( m_activateThemeJob != null )
      m_activateThemeJob.dispose();

    super.dispose();
  }

  /**
   * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
   */
  @Override
  public Object execute( final ExecutionEvent event )
  {
    final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();

    final Shell shell = (Shell) context.getVariable( ISources.ACTIVE_SHELL_NAME );
    final IWorkbenchWindow window = (IWorkbenchWindow) context.getVariable( ISources.ACTIVE_WORKBENCH_WINDOW_NAME );
    final IWorkbenchPage activePage = window == null ? null : window.getActivePage();

    // TODO: do not program against a fixed view id; better try to adapt part to MapPanel instead: this works for every
    // part containing a map-panel
    final IViewPart view = activePage == null ? null : activePage.findView( MapView.ID );

    if( m_featureType != null && view != null && view instanceof MapView )
    {
      final MapView mapView = (MapView) view;
      final MapPanel mapPanel = mapView.getMapPanel();

      MapModellHelper.waitForAndErrorDialog( shell, mapPanel, "Thema aktivieren", "Warten auf Karte gescheitert" );

      final IMapModell mapModell = mapPanel.getMapModell();

      if( mapModell == null )
        return Status.CANCEL_STATUS;

      final String featureType = m_featureType;
      m_activateThemeJob = new ActivateThemeJob( mapModell, Messages.getString( "ThemeContextHandler.3" ), featureType );
      m_activateThemeJob.setRule( mapView.getSchedulingRule().getActivateLayerSchedulingRule() );
      m_activateThemeJob.setUser( true );
      m_activateThemeJob.schedule();
    }

    return Status.OK_STATUS;
  }
}
