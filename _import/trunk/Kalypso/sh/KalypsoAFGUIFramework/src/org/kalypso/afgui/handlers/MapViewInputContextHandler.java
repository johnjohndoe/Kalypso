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
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.afgui.i18n.Messages;
import org.kalypso.afgui.scenarios.IScenario;
import org.kalypso.afgui.scenarios.ScenarioHelper;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.widgets.IWidgetManager;
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
  private final String m_url;

  /**
   * Creates a new {@link MapViewInputContextHandler} that loads the given input file
   */
  public MapViewInputContextHandler( final Properties properties )
  {
    m_url = properties.getProperty( KalypsoContextHandlerFactory.PARAM_INPUT );

    Assert.isNotNull( m_url, Messages.getString("org.kalypso.afgui.handlers.MapViewInputContextHandler.0") ); //$NON-NLS-1$
  }

  /**
   * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
   */
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    IFile iMap;

    final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();

    /* project absolute location */
    if( m_url.startsWith( "project://" ) ) //$NON-NLS-1$
    {
      final String url = m_url.substring( 10 );
      final IProject project = KalypsoAFGUIFrameworkPlugin.getDefault().getActiveWorkContext().getCurrentCase().getProject();

      iMap = project.getFile( url );
    }
    /* base scenario relative location */
    else if( m_url.startsWith( "base://" ) ) //$NON-NLS-1$
    {
      try
      {
        final String url = m_url.substring( 7 );
        final IScenario caze = KalypsoAFGUIFrameworkPlugin.getDefault().getActiveWorkContext().getCurrentCase();
        final IScenario root = ScenarioHelper.resolveRootScenario( caze );

        final IFolder rootFolder = root.getFolder();
        iMap = rootFolder.getFile( url );
      }
      catch( final CoreException e )
      {
        throw new ExecutionException( Messages.getString("org.kalypso.afgui.handlers.MapViewInputContextHandler.3") ); //$NON-NLS-1$
      }
    }
    /* current scenario relative location */
    else
    {

      final IFolder folder = (IFolder) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_FOLDER_NAME );
      if( folder == null )
      {
        throw new ExecutionException( Messages.getString("org.kalypso.afgui.handlers.MapViewInputContextHandler.4") ); //$NON-NLS-1$
      }
      else if( m_url == null )
      {
        throw new ExecutionException( Messages.getString("org.kalypso.afgui.handlers.MapViewInputContextHandler.5") ); //$NON-NLS-1$
      }

      // find file in active scenario folder
      iMap = folder.getFile( m_url );
    }

    // find map view
    final IWorkbenchWindow window = (IWorkbenchWindow) context.getVariable( ISources.ACTIVE_WORKBENCH_WINDOW_NAME );
    final IWorkbenchPage activePage = window == null ? null : window.getActivePage();
    final IViewPart view = activePage == null ? null : activePage.findView( MapView.ID );

    if( view == null || !(view instanceof MapView) )
    {
      throw new ExecutionException( Messages.getString("org.kalypso.afgui.handlers.MapViewInputContextHandler.6") ); //$NON-NLS-1$
    }
    else
    {
      // there is a map view and a file
      final MapView mapView = (MapView) view;
      final IMapPanel mapPanel = (IMapPanel) mapView.getAdapter( IMapPanel.class );

      // only load if the file is not currently shown
      final IFile currentFile = mapView.getFile();
      if( !iMap.equals( currentFile ) )
      {
        mapView.startLoadJob( iMap );
      }

      // make sure that no theme is active when initializing this context
      final Job unsetActiveThemeJob = new Job( "" ) //$NON-NLS-1$
      {
        @Override
        protected IStatus run( final IProgressMonitor monitor )
        {
          final IMapModell mapModell = mapPanel.getMapModell();
          if( mapModell == null )
          {
            return Status.CANCEL_STATUS;
          }

          mapModell.activateTheme( null );
          return Status.OK_STATUS;
        }
      };
      unsetActiveThemeJob.setRule( mapView.getSchedulingRule().getActivateLayerSchedulingRule() );
      unsetActiveThemeJob.schedule();

      // make sure that no widget is active
      final IWidgetManager widgetManager = mapPanel.getWidgetManager();
      widgetManager.setActualWidget( null );

      return Status.OK_STATUS;
    }
  }
}
