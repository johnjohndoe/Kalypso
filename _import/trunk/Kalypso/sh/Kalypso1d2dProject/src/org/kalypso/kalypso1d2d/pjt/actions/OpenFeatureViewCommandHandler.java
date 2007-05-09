/**
 * 
 */
package org.kalypso.kalypso1d2d.pjt.actions;

import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExecutableExtension;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.kalypso1d2d.pjt.SzenarioSourceProvider;
import org.kalypso.kalypso1d2d.pjt.i18n.Messages;
import org.kalypso.ui.editor.featureeditor.FeatureTemplateView;


/**
 * Opens the feature view on a given template
 * 
 * @author Stefan Kurzbach
 */
public class OpenFeatureViewCommandHandler extends AbstractHandler implements IExecutableExtension
{
  private static final String PARAM_RESOURCE = "org.kalypso.kalypso1d2d.pjt.OpenFeatureViewCommand.resource"; //$NON-NLS-1$  

  private String m_resource;

  /**
   * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
   */
  @SuppressWarnings("unchecked")//$NON-NLS-1$
  @Override
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();

    try
    {
      if( m_resource == null )
      {
        throw new CoreException( StatusUtilities.createErrorStatus( Messages.getString( "org.kalypso.kalypso1d2d.pjt.actions.OpenMapViewCommandHandler.1" ) ) );//$NON-NLS-1$
      }

      final IWorkbenchWindow activeWorkbenchWindow = (IWorkbenchWindow) context.getVariable( ISources.ACTIVE_WORKBENCH_WINDOW_NAME );
      final IFolder szenarioFolder = (IFolder) context.getVariable( SzenarioSourceProvider.ACTIVE_SZENARIO_FOLDER_NAME );

      final IFolder folder = SzenarioSourceProvider.findModelContext( szenarioFolder, m_resource );
      IFile file = null;
      if( folder != null )
        file = folder.getFile( m_resource );

      if( file != null && file.exists() && activeWorkbenchWindow != null )
      {
        final IWorkbenchPage workbenchPage = activeWorkbenchWindow.getActivePage();
        final FeatureTemplateView featureView = (FeatureTemplateView) workbenchPage.showView( FeatureTemplateView.ID );
        featureView.loadFromTemplate( file );
      }
    }
    catch( final CoreException e )
    {
      throw new ExecutionException( "Could not open feature template file" + m_resource, e );
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
    }
  }
}
