/**
 * 
 */
package de.renew.workflow.contexts;

import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExecutableExtension;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;

/**
 * Opens the map view on a given resource and activates a given layer
 * 
 * @author Stefan Kurzbach
 */
public class ViewContextHandler extends AbstractHandler implements IExecutableExtension
{
  public static final String CONTEXT_VIEW_ID = "org.kalypso.afgui.contexts.view"; //$NON-NLS-1$

  private String m_viewId;

  public ViewContextHandler( )
  {
  }

  public ViewContextHandler( final String viewId )
  {
    m_viewId = viewId;
  }

  /**
   * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
   */
  @SuppressWarnings("unchecked")
  @Override
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();

    final IWorkbenchWindow activeWorkbenchWindow = (IWorkbenchWindow) context.getVariable( ISources.ACTIVE_WORKBENCH_WINDOW_NAME );
    if( activeWorkbenchWindow != null && m_viewId != null )
    {
      final IWorkbenchPage workbenchPage = activeWorkbenchWindow.getActivePage();
      try
      {
        workbenchPage.showView( m_viewId, null, IWorkbenchPage.VIEW_VISIBLE );
      }
      catch( final PartInitException e )
      {
        throw new ExecutionException( Messages.getString( "ViewContextHandler.0" ) + m_viewId, e ); //$NON-NLS-1$
      }
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
      m_viewId = (String) parameterMap.get( CONTEXT_VIEW_ID );
    }
  }
}
