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
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.operation.IRunnableContext;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.contribs.eclipse.EclipseRCPContributionsPlugin;

/**
 * Helper-Class for IRunnableContext
 * 
 * @author belger
 */
public abstract class RunnableContextHelper implements IRunnableWithProgress
{
  private final IRunnableContext m_context;
  private IStatus m_status = Status.OK_STATUS;

  public RunnableContextHelper( final IRunnableContext context )
  {
    m_context = context;
  }

  public IStatus runAndHandleOperation( final Shell shell, final boolean fork, final boolean cancelable,
      final String title, final String message )
  {
    try
    {
      m_context.run( fork, cancelable, this );
    }
    catch( final InvocationTargetException e )
    {
      e.printStackTrace();

      final Throwable targetException = e.getTargetException();

      if( targetException instanceof CoreException )
        m_status = ( (CoreException)targetException ).getStatus();
      else
      {
        final String locmsg = targetException.getLocalizedMessage();
        final String msg = locmsg == null ? "" : locmsg;
        m_status = new Status( IStatus.ERROR, EclipseRCPContributionsPlugin.getID(), 0, msg, targetException );
      }
    }
    catch( final InterruptedException e )
    {
      e.printStackTrace();
      
      return Status.CANCEL_STATUS;
    }
    
    ErrorDialog.openError( shell, title, message, m_status );
    
    return m_status;
  }
  
  /**
   * @see org.eclipse.jface.operation.IRunnableWithProgress#run(org.eclipse.core.runtime.IProgressMonitor)
   */
  public void run( final IProgressMonitor monitor ) throws InvocationTargetException, InterruptedException
  {
    try
    {
      m_status = execute( monitor );
    }
    catch( final CoreException e )
    {
      throw new InvocationTargetException( e );
    }
  }

  protected abstract IStatus execute( final IProgressMonitor monitor ) throws CoreException, InvocationTargetException, InterruptedException;
}
