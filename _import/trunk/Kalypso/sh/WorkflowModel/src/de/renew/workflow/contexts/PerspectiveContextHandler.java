/**
 * 
 */
package de.renew.workflow.contexts;

import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExecutableExtension;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.WorkbenchException;

/**
 * Opens the map view on a given resource and activates a given layer
 * 
 * @author Stefan Kurzbach
 */
public class PerspectiveContextHandler extends AbstractHandler implements IExecutableExtension
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

  @Override
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    final IWorkbenchWindow activeWorkbenchWindow = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
    if( activeWorkbenchWindow == null || m_perspectiveId == null )
      throw new ExecutionException( "Could not activate perspective: " + m_perspectiveId );

    try
    {
      final IWorkbench workbench = activeWorkbenchWindow.getWorkbench();
      workbench.showPerspective( m_perspectiveId, activeWorkbenchWindow );
      return Status.OK_STATUS;
    }
    catch( final WorkbenchException e )
    {
      throw new ExecutionException( Messages.getString( "PerspectiveContextHandler.0" ) + m_perspectiveId, e ); //$NON-NLS-1$
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
      final Map< ? , ? > parameterMap = (Map< ? , ? >) data;
      m_perspectiveId = (String) parameterMap.get( CONTEXT_PERSPECTIVE_ID );
    }
  }
}
