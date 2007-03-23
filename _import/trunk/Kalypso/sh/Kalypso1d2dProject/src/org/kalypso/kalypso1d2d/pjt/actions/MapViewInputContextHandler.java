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
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExecutableExtension;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.afgui.workflow.ViewContextHandler;
import org.kalypso.kalypso1d2d.pjt.SzenarioSourceProvider;
import org.kalypso.kalypso1d2d.pjt.i18n.Messages;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ui.views.map.MapView;

import de.renew.workflow.WorkflowCommandHandler;

/**
 * Loads a template file in the current map view. Requires that the current context contains the map view. Use a
 * {@link ViewContextHandler} for this purpose.
 * 
 * @author Stefan Kurzbach
 */
public class MapViewInputContextHandler extends WorkflowCommandHandler implements IHandler, IExecutableExtension
{
  public static final String MAPVIEW_INPUT = "org.kalypso.kalypso1d2d.pjt.contexts.mapViewInput"; //$NON-NLS-1$

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
  @SuppressWarnings("unchecked")//$NON-NLS-1$
  @Override
  protected IStatus executeInternal( final ExecutionEvent event )
  {
    final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();

    final IFolder szenarioFolder = (IFolder) context.getVariable( SzenarioSourceProvider.ACTIVE_SZENARIO_FOLDER_NAME );
    final IFolder folder = SzenarioSourceProvider.findModelContext( szenarioFolder, m_mapViewInput );
    IFile file = null;
    if( folder != null && m_mapViewInput != null )
      file = folder.getFile( m_mapViewInput );

    final Object view = context.getVariable( ViewContextHandler.CONTEXT_VIEW_ID );

    if( file != null && file.exists() && view != null && view instanceof MapView )
    {
      final MapView mapView = (MapView) view;
      final MapPanel mapPanel = (MapPanel) mapView.getAdapter( MapPanel.class );

      final IFile currentFile = mapView.getFile();
      if( !file.equals( currentFile ) )
      {
        logger.info( Messages.getString( "org.kalypso.kalypso1d2d.pjt.actions.OpenMapViewCommandHandler.2" ) + file ); //$NON-NLS-1$
        mapView.startLoadJob( file );
      }

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
    else
    {
      logger.severe( "Could not initialize with data of type " + data.getClass().getName() ); //$NON-NLS-1$
    }
  }
}
