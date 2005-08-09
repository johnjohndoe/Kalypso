/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.contribs.eclipse.jface.operation;

import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ProgressMonitorDialog;
import org.eclipse.jface.operation.IRunnableContext;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.contribs.eclipse.EclipseRCPContributionsPlugin;

/**
 * Helper-Class for IRunnableContext
 * 
 * @author belger
 */
public final class RunnableContextHelper
{
  public static final class CoreRunnableWrapper implements IRunnableWithProgress
  {
    private final ICoreRunnableWithProgress m_runnable;

    private IStatus m_status = Status.OK_STATUS;

    public CoreRunnableWrapper( final ICoreRunnableWithProgress runnable )
    {
      m_runnable = runnable;
    }

    public void run( final IProgressMonitor monitor ) throws InvocationTargetException, InterruptedException
    {
      try
      {
        m_status = m_runnable.execute( monitor );
      }
      catch( final CoreException e )
      {
        throw new InvocationTargetException( e );
      }
    }

    public IStatus getStatus()
    {
      return m_status;
    }
  }

  private final IRunnableContext m_context;

  private RunnableContextHelper( final IRunnableContext context )
  {
    m_context = context;
  }

  /**
   * Transforms any exception into an {@link IStatus}object.
   * <p>
   * If the exception is an {@link InvocationTargetException}the inner exception is wrapped instead.
   * </p>
   * <p>
   * If the exception is a {@link CoreException}its status is returned.
   * </p>
   * 
   * 
   * @throws NullPointerException
   *           If <code>t</code> is null.
   */
  public static IStatus statusFromThrowable( final Throwable t )
  {
    if( t instanceof InvocationTargetException )
      return statusFromThrowable( ( (InvocationTargetException)t ).getCause() );
    if( t instanceof CoreException )
      return ( (CoreException)t ).getStatus();

    final String locmsg = t.getLocalizedMessage();
    final String msg = locmsg == null ? "" : locmsg;
    return new Status( IStatus.ERROR, EclipseRCPContributionsPlugin.getID(), 0, msg, t );
  }

  /**
   * Runs the given runnable in the given context, but catches all (event runtime-) exception and turns them into a
   * {@Link IStatus}object.
   */
  public IStatus execute( final boolean fork, final boolean cancelable, final IRunnableWithProgress runnable )
  {
    try
    {
      m_context.run( fork, cancelable, runnable );
    }
    catch( final Throwable t )
    {
      // TODO log stacktrace somewhere
      return statusFromThrowable( t );
    }

    if( runnable instanceof CoreRunnableWrapper )
      return ( (CoreRunnableWrapper)runnable ).getStatus();

    return Status.OK_STATUS;
  }

  /**
   * Runs the given runnable in the given context, but catches all (event runtime-) exception and turns them into a
   * {@Link IStatus}object.
   */
  public IStatus execute( final boolean fork, final boolean cancelable, final ICoreRunnableWithProgress runnable )
  {
    final IRunnableWithProgress innerRunnable = new CoreRunnableWrapper( runnable );

    return execute( fork, cancelable, innerRunnable );
  }

  /**
   * Runs the given runnable in the given context, but catches all (event runtime-) exception and turns them into a
   * {@Link IStatus}object.
   */
  public static IStatus execute( final IRunnableContext context, final boolean fork, final boolean cancelable,
      final IRunnableWithProgress runnable )
  {
    final RunnableContextHelper helper = new RunnableContextHelper( context );
    return helper.execute( fork, cancelable, runnable );
  }

  /**
   * Runs the given runnable in the given context, but catches all (event runtime-) exception and turns them into a
   * {@Link IStatus}object.
   */
  public static IStatus execute( final IRunnableContext context, final boolean fork, final boolean cancelable,
      final ICoreRunnableWithProgress runnable )
  {
    final RunnableContextHelper helper = new RunnableContextHelper( context );
    return helper.execute( fork, cancelable, runnable );
  }

  /**
   * Runs a runnable in a progress monitor dialog.
   */
  public final static void executeInProgressDialog( final Shell shell, final ICoreRunnableWithProgress runnable,
      final IErrorHandler errorHandler )
  {
    // run the execute method in a Progress-Dialog
    final ProgressMonitorDialog dialog = new ProgressMonitorDialog( shell );
    final IStatus status = execute( dialog, false, true, runnable );
    errorHandler.handleError( shell, status );
  }
}
