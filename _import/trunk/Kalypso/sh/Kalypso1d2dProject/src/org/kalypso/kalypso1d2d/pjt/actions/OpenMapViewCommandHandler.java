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
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.kalypso1d2d.pjt.SzenarioSourceProvider;
import org.kalypso.kalypso1d2d.pjt.i18n.Messages;
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
  public static final String PARAM_RESOURCE = "org.kalypso.kalypso1d2d.pjt.OpenMapViewCommand.resource"; //$NON-NLS-1$

  public static final String PARAM_LAYER_FEATURE_TYPE = "org.kalypso.kalypso1d2d.pjt.OpenMapViewCommand.layer"; //$NON-NLS-1$

  public static final String NO_LAYER = "NO_LAYER";

  private String m_resource;

  private String m_layerContext;

  private IMapModell m_mapModell;

  private MapView m_mapView;

  /**
   * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
   */
  @SuppressWarnings("unchecked")//$NON-NLS-1$
  @Override
  protected IStatus executeInternal( final ExecutionEvent event ) throws CoreException
  {
    final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();

    if( m_resource == null )
    {
      throw new CoreException( StatusUtilities.createErrorStatus( Messages.getString( "org.kalypso.kalypso1d2d.pjt.actions.OpenMapViewCommandHandler.1" ) ) ); //$NON-NLS-1$
    }

    final IWorkbenchWindow activeWorkbenchWindow = (IWorkbenchWindow) context.getVariable( ISources.ACTIVE_WORKBENCH_WINDOW_NAME );
    final IFolder szenarioFolder = (IFolder) context.getVariable( SzenarioSourceProvider.ACTIVE_SZENARIO_FOLDER_NAME );

    final IFolder folder = SzenarioSourceProvider.findModelContext( szenarioFolder, m_resource );
    IFile file = null;
    if( folder != null )
      file = folder.getFile( m_resource );

    if( file != null && file.exists() && activeWorkbenchWindow != null )
    {
      logger.info( Messages.getString( "org.kalypso.kalypso1d2d.pjt.actions.OpenMapViewCommandHandler.2" ) + file ); //$NON-NLS-1$

      final IWorkbenchPage workbenchPage = activeWorkbenchWindow.getActivePage();
      m_mapView = (MapView) workbenchPage.showView( MapView.ID );

      final MapPanel mapPanel = (MapPanel) m_mapView.getAdapter( MapPanel.class );

      final IFile currentFile = m_mapView.getFile();
      if( !file.equals( currentFile ) )
      {
        m_mapView.startLoadJob( file );
      }

      mapPanel.getWidgetManager().setActualWidget( null );

      final IMapModell mapModell = mapPanel.getMapModell();
      if( mapModell == null )
        return Status.OK_STATUS; // OK status ok?

      final String layerContext = m_layerContext;
      m_mapModell = mapModell;

      final Job job = new Job( Messages.getString( "org.kalypso.kalypso1d2d.pjt.actions.OpenMapViewCommandHandler.3" ) ) //$NON-NLS-1$
      {
        @Override
        protected IStatus run( final IProgressMonitor monitor )
        {
          if( layerContext != null )
          {
            final IKalypsoTheme activeTheme = mapModell.getActiveTheme();
            if( activeTheme != null && layerContext.equals( NO_LAYER ) )
            {
              mapModell.activateTheme( null );
            }
            else if( !layerContext.equals( activeTheme != null ? activeTheme.getContext() : null ) )
            {
              final IKalypsoTheme[] allThemes = mapModell.getAllThemes();
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
          }

          return Status.OK_STATUS;
        }
      };
      job.setRule( m_mapView.getSchedulingRule().getActivateLayerSchedulingRule() );
      job.setUser( true );
      job.schedule();
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
      m_layerContext = (String) parameterMap.get( PARAM_LAYER_FEATURE_TYPE );
    }
    else
    {
      logger.severe( "Could not initialize with data of type " + data.getClass().getName() ); //$NON-NLS-1$
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
    try
    {
      final String themeContext = themeToActivate.getContext();
      if( m_layerContext != null && m_layerContext.equals( themeContext ) )
      {
        logger.info( themeToActivate + " theme activated with feature type " + m_layerContext ); //$NON-NLS-1$
        m_mapModell.activateTheme( themeToActivate );
      }
    }
    catch( final Throwable t )
    {
      t.printStackTrace();
    }
  }
}
