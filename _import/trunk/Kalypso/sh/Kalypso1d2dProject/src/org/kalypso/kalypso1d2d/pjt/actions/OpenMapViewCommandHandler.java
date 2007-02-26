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
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.kalypso1d2d.pjt.SzenarioSourceProvider;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.IKalypsoThemeListener;
import org.kalypso.ogc.gml.KalypsoThemeEvent;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ui.views.map.MapView;

import de.renew.workflow.WorkflowCommandHandler;

/**
 * Opens the map view on a given resource and activates a given layer
 * 
 * @author Stefan Kurzbach
 */
public class OpenMapViewCommandHandler extends WorkflowCommandHandler implements IHandler, IExecutableExtension, IKalypsoThemeListener
{

  private static final String PARAM_RESOURCE = "org.kalypso.kalypso1d2d.pjt.OpenMapViewCommand.resource";

  private static final String PARAM_LAYER_FEATURE_TYPE = "org.kalypso.kalypso1d2d.pjt.OpenMapViewCommand.layer";

  private String m_resource;

  String m_featureType;

  IMapModell m_mapModell;

  private MapView m_mapView;

  /**
   * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
   */
  @SuppressWarnings("unchecked")
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

    if( file.exists() && activeWorkbenchWindow != null )
    {
      final IWorkbenchPage workbenchPage = activeWorkbenchWindow.getActivePage();
      m_mapView = (MapView) workbenchPage.showView( MapView.ID );

      final MapPanel mapPanel = (MapPanel) m_mapView.getAdapter( MapPanel.class );

      if( !file.equals( m_mapView.getFile() ) )
      {
        m_mapView.startLoadJob( file );
      }

      if( m_featureType != null )
      {
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

            m_mapModell = mapPanel.getMapModell();
            final IKalypsoTheme activeTheme = m_mapModell.getActiveTheme();
            if( !m_featureType.equals( activeTheme.getContext() ) )
            {
              final IKalypsoTheme[] allThemes = m_mapModell.getAllThemes();
              for( final IKalypsoTheme theme : allThemes )
              {
                if( !theme.isLoaded() )
                {
                  theme.addKalypsoThemeListener( OpenMapViewCommandHandler.this );
                }
                else
                {
                  maybeActivateTheme( theme );
                }
              }
            }
            return Status.OK_STATUS;
          }
        };
        job.setUser( true );
        job.schedule();
      }

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

  /**
   * @see org.kalypso.ogc.gml.IKalypsoThemeListener#kalypsoThemeChanged(org.kalypso.ogc.gml.KalypsoThemeEvent)
   */
  public void kalypsoThemeChanged( final KalypsoThemeEvent event )
  {
    if( event.isType( KalypsoThemeEvent.CONTEXT_CHANGED ) )
    {
      final IKalypsoTheme themeToActivate = event.getSource();
      maybeActivateTheme( themeToActivate );
      themeToActivate.removeKalypsoThemeListener( this );
    }
  }

  void maybeActivateTheme( final IKalypsoTheme themeToActivate )
  {
    final String themeContext = themeToActivate.getContext();
    if( m_featureType.equals( themeContext ) )
    {
      logger.info( themeToActivate + " theme activated with feature type " + m_featureType );
      m_mapModell.activateTheme( themeToActivate );
      m_mapView.setCustomName( themeToActivate.getName() );
    }
  }
}
