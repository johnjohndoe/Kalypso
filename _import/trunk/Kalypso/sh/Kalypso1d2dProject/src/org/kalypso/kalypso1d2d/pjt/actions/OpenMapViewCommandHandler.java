/**
 * 
 */
package org.kalypso.kalypso1d2d.pjt.actions;

import java.util.Map;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.IHandler;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExecutableExtension;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IViewReference;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.kalypso1d2d.pjt.SzenarioSourceProvider;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ui.views.map.MapView;

import de.renew.workflow.WorkflowCommandHandler;

/**
 * Opens the map view on a given resource and activates a given layer
 * 
 * @author Stefan Kurzbach
 */
public class OpenMapViewCommandHandler extends WorkflowCommandHandler implements IHandler, IExecutableExtension
{

  private static final String PARAM_RESOURCE = "org.kalypso.kalypso1d2d.pjt.OpenMapViewCommand.resource";

  private static final String PARAM_LAYER_FEATURE_TYPE = "org.kalypso.kalypso1d2d.pjt.OpenMapViewCommand.layer";

  private String m_resource;

  String m_featureType;

  /**
   * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
   */
  @Override
  protected IStatus executeInternal( final ExecutionEvent event ) throws CoreException
  {
    final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();

    if( m_resource == null )
    {
      throw new CoreException( StatusUtilities.createErrorStatus( "Resource parameter was null." ) );
    }

    final IWorkbenchWindow activeWorkbenchWindow = (IWorkbenchWindow) context.getVariable( ISources.ACTIVE_WORKBENCH_WINDOW_NAME );
    final IFolder szenarioPath = (IFolder) context.getVariable( SzenarioSourceProvider.ACTIVE_SZENARIO_FOLDER_NAME );

    final IFile file = szenarioPath.getFile( new Path( m_resource ) );

    logger.info( "Opening " + file );

    if( file.exists() )
    {
      final IWorkbenchPage workbenchPage = activeWorkbenchWindow.getActivePage();
      final IViewReference[] viewReferences = workbenchPage.getViewReferences();
      for( final IViewReference reference : viewReferences )
      {
        if( !shouldKeepView( reference ) )
        {
          logger.info( "Hiding view " + reference.getPartName() );
          workbenchPage.hideView( reference );
        }
      }
      final MapView mapView = (MapView) workbenchPage.showView( MapView.ID );

      // final SzenarioDataProvider dataProvider = (SzenarioDataProvider) context.getVariable(
      // SzenarioSourceProvider.ACTIVE_SZENARIO_DATA_PROVIDER_NAME );
      // TODO check dirty-state of workspace and ask if editor should be saved

      final Job loadMapJob = mapView.loadMap( file );
      final MapPanel mapPanel = (MapPanel) mapView.getAdapter( MapPanel.class );
      final Job job = new Job( "Activate layer..." )
      {

        @Override
        protected IStatus run( final IProgressMonitor monitor )
        {
          try
          {
            Job.getJobManager().join( MapView.JOB_FAMILY, monitor );
          }
          catch( final OperationCanceledException e )
          {
            return StatusUtilities.statusFromThrowable( e );
          }
          catch( final InterruptedException e )
          {
            return StatusUtilities.statusFromThrowable( e );
          }

          final IMapModell mapModell = mapPanel.getMapModell();
          if( m_featureType != null )
          {
            final IKalypsoTheme[] allThemes = mapModell.getAllThemes();
            IKalypsoTheme themeToActivate = null;

            for( IKalypsoTheme theme : allThemes )
            {
              final String themeContext = theme.getContext();
              if( m_featureType.equals( themeContext ) )
              {
                if( themeToActivate != null )
                {
                  logger.warning( theme.getName() + " theme found. More than one theme with the same feature type: " + m_featureType );
                }
                themeToActivate = theme;
              }
            }

            if( themeToActivate != null )
            {
              logger.info( themeToActivate.getName() + " theme activated with feature type " + m_featureType );
              mapModell.activateTheme( themeToActivate );
            }
            else
            {
              logger.warning( "Could not activate theme with feature type " + m_featureType + ". Not found in " + file );
            }
          }
          return Status.OK_STATUS;
        }
      };
      job.setUser( true );
      job.schedule( 500 );

    }
    return Status.OK_STATUS;
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
      m_resource = (String) parameterMap.get( PARAM_RESOURCE );
      m_featureType = (String) parameterMap.get( PARAM_LAYER_FEATURE_TYPE );
    }
    else
    {
      logger.severe( "Could not initialize with data of type " + data.getClass().getName() );
    }
  }

  private boolean shouldKeepView( final IViewReference reference )
  {
    return true;

    // final String viewId = reference.getId();
    // if( WorkflowView.ID.equals( viewId ) )
    // {
    // return true;
    // }
    // else if( SimulationModelDBView.ID.equals( viewId ) )
    // {
    // return true;
    // }
    // else
    // {
    // return false;
    // }
  }
}
