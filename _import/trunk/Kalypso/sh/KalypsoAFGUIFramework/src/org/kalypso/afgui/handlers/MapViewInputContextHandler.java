/**
 *
 */
package org.kalypso.afgui.handlers;

import java.util.Properties;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.widgets.WidgetManager;
import org.kalypso.ui.views.map.MapView;

import de.renew.workflow.contexts.ICaseHandlingSourceProvider;

/**
 * Loads a template file in the current map view. Requires that the current context contains the map view. Use a
 * {@link ViewContextHandler} for this purpose.
 * 
 * @author Stefan Kurzbach
 */
public class MapViewInputContextHandler extends AbstractHandler
{
  private final String m_mapViewInput;

  /**
   * Creates a new {@link MapViewInputContextHandler} that loads the given input file
   */
  public MapViewInputContextHandler( final Properties properties )
  {
    m_mapViewInput = properties.getProperty( KalypsoContextHandlerFactory.PARAM_INPUT );

    Assert.isNotNull( m_mapViewInput, "Parameter 'input' not set for mapViewInputContext" );
  }

  /**
   * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
   */
  @Override
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();
    final IFolder folder = (IFolder) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_FOLDER_NAME );
    if( folder == null )
    {
      throw new ExecutionException( "Kein Szenario aktiv oder Szenarioordner nicht gefunden." );
    }
    else if( m_mapViewInput == null )
    {
      throw new ExecutionException( "Keine Kartenvorlage angegeben." );
    }

    // find file in active scenario folder
    final IFile file = folder.getFile( m_mapViewInput );

    // find map view
    final IWorkbenchWindow window = (IWorkbenchWindow) context.getVariable( ISources.ACTIVE_WORKBENCH_WINDOW_NAME );
    final IWorkbenchPage activePage = window == null ? null : window.getActivePage();
    final IViewPart view = activePage == null ? null : activePage.findView( MapView.ID );

    if( view == null || !(view instanceof MapView) )
    {
      throw new ExecutionException( "Es wurde keine ge�ffnete Karte gefunden." );
    }
    else
    {
      // there is a map view and a file

      final MapView mapView = (MapView) view;
      final MapPanel mapPanel = (MapPanel) mapView.getAdapter( MapPanel.class );

      // only load if the file is not currently shown
      final IFile currentFile = mapView.getFile();
      if( !file.equals( currentFile ) )
      {
        mapView.startLoadJob( file );
      }

      // make sure that no theme is active when initializing this context
      final Job unsetActiveThemeJob = new Job( "" ) //$NON-NLS-1$
      {
        @Override
        protected IStatus run( final IProgressMonitor monitor )
        {
          final IMapModell mapModell = mapPanel.getMapModell();
          if( mapModell == null )
            return Status.CANCEL_STATUS;

          mapModell.activateTheme( null );
          return Status.OK_STATUS;
        }
      };
      unsetActiveThemeJob.setRule( mapView.getSchedulingRule().getActivateLayerSchedulingRule() );
      unsetActiveThemeJob.schedule();

      // make sure that no widget is active
      final WidgetManager widgetManager = mapPanel.getWidgetManager();
      widgetManager.setActualWidget( null );

      return Status.OK_STATUS;
    }
  }
}
