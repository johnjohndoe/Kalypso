/**
 * 
 */
package org.kalypso.afgui.workflow;

import java.util.Map;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.IHandler;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExecutableExtension;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWindow;

import de.renew.workflow.WorkflowCommandHandler;

/**
 * Opens the map view on a given resource and activates a given layer
 * 
 * @author Stefan Kurzbach
 */
public class PerspectiveContextHandler extends WorkflowCommandHandler implements IHandler, IExecutableExtension
{
  public static final String CONTEXT_PERSPECTIVE_ID = "org.kalypso.afgui.contexts.perspective"; //$NON-NLS-1$

  private String m_perspectiveId;

  public PerspectiveContextHandler( )
  {
  }

  public PerspectiveContextHandler( final String perspectiveId )
  {
    m_perspectiveId = perspectiveId;
  }

  /**
   * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
   */
  @SuppressWarnings("unchecked")//$NON-NLS-1$
  @Override
  protected IStatus executeInternal( final ExecutionEvent event ) throws CoreException
  {
    final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();

    final IWorkbenchWindow activeWorkbenchWindow = (IWorkbenchWindow) context.getVariable( ISources.ACTIVE_WORKBENCH_WINDOW_NAME );
    if( activeWorkbenchWindow != null && m_perspectiveId != null )
    {
      final IWorkbench workbench = activeWorkbenchWindow.getWorkbench();
      workbench.showPerspective( m_perspectiveId, activeWorkbenchWindow );
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
      m_perspectiveId = (String) parameterMap.get( CONTEXT_PERSPECTIVE_ID );
    }
    else
    {
      logger.severe( "Could not initialize with data of type " + data.getClass().getName() ); //$NON-NLS-1$
    }
  }
}
