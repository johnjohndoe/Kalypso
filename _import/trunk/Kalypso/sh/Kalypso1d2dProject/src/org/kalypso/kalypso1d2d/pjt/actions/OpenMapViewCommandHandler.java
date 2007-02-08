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
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IViewReference;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.kalypso1d2d.pjt.SzenarioSourceProvider;
import org.kalypso.kalypso1d2d.pjt.views.SimulationModelDBView;
import org.kalypso.kalypso1d2d.pjt.views.WorkflowView;
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
  private String m_resource;

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
    logger.info( "Opening " + m_resource );
    final IWorkbenchWindow activeWorkbenchWindow = (IWorkbenchWindow) context.getVariable( ISources.ACTIVE_WORKBENCH_WINDOW_NAME );
    final IFolder szenarioPath = (IFolder) context.getVariable( SzenarioSourceProvider.ACTIVE_SZENARIO_FOLDER_NAME );

    final IFile file = szenarioPath.getFile( new Path( m_resource ) );

    if( file.exists() )
    {
      final IWorkbenchPage workbenchPage = activeWorkbenchWindow.getActivePage();
      final IViewReference[] viewReferences = workbenchPage.getViewReferences();
      for( final IViewReference reference : viewReferences )
      {
        if( !shouldKeepView( reference ) )
          workbenchPage.hideView( reference );
      }
      final MapView mapView = (MapView) workbenchPage.showView( MapView.ID );
      mapView.loadMap( file );
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
      Map parameterMap = (Map) data;
      m_resource = (String) parameterMap.get( PARAM_RESOURCE );      
    }
  }

  private boolean shouldKeepView( final IViewReference reference )
  {
    final String viewId = reference.getId();
    if( WorkflowView.ID.equals( viewId ) )
    {
      return true;
    }
    else if( SimulationModelDBView.ID.equals( viewId ) )
    {
      return true;
    }
    else
    {
      return false;
    }
  }
}
