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
import org.eclipse.ui.ISources;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.kalypso.kalypso1d2d.pjt.SzenarioSourceProvider;
import org.kalypso.ui.editor.featureeditor.FeatureTemplateView;

import de.renew.workflow.WorkflowCommandHandler;

/**
 * Loads a template file in the current feature view. Requires that the current context contains the feature view. Use a
 * {@link ViewContextHandler} for this purpose.
 * 
 * @author Stefan Kurzbach
 */
public class FeatureViewInputContextHandler extends WorkflowCommandHandler implements IHandler, IExecutableExtension
{
  public static final String FEATUREVIEW_INPUT = "org.kalypso.kalypso1d2d.pjt.contexts.featureViewInput"; //$NON-NLS-1$

  private String m_featureViewInput;

  /**
   * Creates a new {@link FeatureViewInputContextHandler} that loads the given input file
   */
  public FeatureViewInputContextHandler( final String input )
  {
    m_featureViewInput = input;
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
    final IFolder folder = SzenarioSourceProvider.findModelContext( szenarioFolder, m_featureViewInput );
    IFile file = null;
    if( folder != null && m_featureViewInput != null )
      file = folder.getFile( m_featureViewInput );

    final IWorkbenchWindow window = (IWorkbenchWindow) context.getVariable( ISources.ACTIVE_WORKBENCH_WINDOW_NAME );
    final IViewPart view = window.getActivePage().findView( FeatureTemplateView.ID );

    if( file != null && file.exists() && view != null && view instanceof FeatureTemplateView )
    {
      final FeatureTemplateView mapView = (FeatureTemplateView) view;
      mapView.loadFromTemplate( file );
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
      m_featureViewInput = (String) parameterMap.get( FEATUREVIEW_INPUT );
    }
    else
    {
      logger.severe( "Could not initialize with data of type " + data.getClass().getName() ); //$NON-NLS-1$
    }
  }
}
