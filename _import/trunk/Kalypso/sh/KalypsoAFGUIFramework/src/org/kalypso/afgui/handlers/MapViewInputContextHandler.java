/**
 * 
 */
package org.kalypso.afgui.handlers;

import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExecutableExtension;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ui.views.map.MapView;

import de.renew.workflow.connector.cases.CaseHandlingSourceProvider;

/**
 * Loads a template file in the current map view. Requires that the current context contains the map view. Use a
 * {@link ViewContextHandler} for this purpose.
 * 
 * @author Stefan Kurzbach
 */
public class MapViewInputContextHandler extends AbstractHandler implements IExecutableExtension
{
  public static final String MAPVIEW_INPUT = "org.kalypso.kalypso1d2d.pjt.contexts.mapViewInput";  //$NON-NLS-1$

  private String m_mapViewInput;

  /**
   * Creates a new {@link MapViewInputContextHandler} that loads the given input file
   */
  public MapViewInputContextHandler( final String input )
  {
    m_mapViewInput = input;
  }

  /**
   * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
   */
  @SuppressWarnings("unchecked")  //$NON-NLS-1$
  @Override
  public Object execute( final ExecutionEvent event )
  {
    final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();

    final IFolder szenarioFolder = (IFolder) context.getVariable( CaseHandlingSourceProvider.ACTIVE_CASE_FOLDER_NAME );
    final IFolder folder = SzenarioDataProvider.findModelContext( szenarioFolder, m_mapViewInput );
    IFile file = null;
    if( folder != null && m_mapViewInput != null )
      file = folder.getFile( m_mapViewInput );

    final IWorkbenchWindow window = (IWorkbenchWindow) context.getVariable( ISources.ACTIVE_WORKBENCH_WINDOW_NAME );
    final IWorkbenchPage activePage = window == null ? null : window.getActivePage();
    final IViewPart view = activePage == null ? null : activePage.findView( MapView.ID );

    if( file != null && file.exists() && view != null && view instanceof MapView )
    {
      final MapView mapView = (MapView) view;
      final MapPanel mapPanel = (MapPanel) mapView.getAdapter( MapPanel.class );

      final IFile currentFile = mapView.getFile();
      if( !file.equals( currentFile ) )
      {
        mapView.startLoadJob( file );
      }
      final Job unsetActiveThemeJob = new Job( "" )  //$NON-NLS-1$
      {
        @Override
        protected IStatus run( final IProgressMonitor monitor )
        {
          final IMapModell mapModell = mapPanel.getMapModell();
          if( mapModell != null )
            mapModell.activateTheme( null );
          return Status.OK_STATUS;
        }
      };
      unsetActiveThemeJob.setRule( mapView.getSchedulingRule().getActivateLayerSchedulingRule() );
      unsetActiveThemeJob.schedule();
      mapPanel.getWidgetManager().setActualWidget( null );

      return Status.OK_STATUS;
    }
    else
    {
      return Status.CANCEL_STATUS;
    }
  }

  /**
   * @see org.eclipse.core.runtime.IExecutableExtension#setInitializationData(org.eclipse.core.runtime.IConfigurationElement,
   *      java.lang.String, java.lang.Object)
   */
  public void setInitializationData( final IConfigurationElement config, final String propertyName, final Object data )
  {
    if( data instanceof Map )
    {
      final Map parameterMap = (Map) data;
      m_mapViewInput = (String) parameterMap.get( MAPVIEW_INPUT );
    }
  }
}
