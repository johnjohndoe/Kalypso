package org.kalypso.eclipse.jface.operation;

import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.operation.IRunnableContext;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.swt.widgets.Shell;

/**
 * Helper-Class for IRunnableContext
 * 
 * @author belger
 */
public abstract class RunnableContextHelper implements IRunnableWithProgress
{
  private final IRunnableContext m_context;

  public RunnableContextHelper( final IRunnableContext context )
  {
    m_context = context;
  }
  
  public void runAndHandleOperation( final Shell shell, final String title, final String message )
  {
    try
    {
      m_context.run( true, false, this );
    }
    catch( final InvocationTargetException e )
    {
      e.printStackTrace();

      IStatus status = null;
      String msg = message;

      final Throwable targetException = e.getTargetException();
      if( targetException instanceof CoreException )
        status = ( (CoreException)targetException ).getStatus();
      else
        msg += "\n" + targetException.getLocalizedMessage();

      ErrorDialog.openError( shell, title, msg, status );
    }
    catch( final InterruptedException e )
    {
      e.printStackTrace();
    }
  }
}
