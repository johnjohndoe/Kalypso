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
package org.kalypso.ui.rrm.action;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.handlers.HandlerUtil;
import org.kalypso.contribs.eclipse.core.runtime.HandleDoneJobChangeAdapter;
import org.kalypso.simulation.ui.calccase.ModelNature;
import org.kalypso.ui.rrm.i18n.Messages;

/**
 * @author huebsch
 */
public class SetBasicModelDelegate extends AbstractHandler
{
  /**
   * @see org.eclipse.core.commands.IHandler#execute(org.eclipse.core.commands.ExecutionEvent)
   */
  @Override
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    final IWorkbenchWindow window = HandlerUtil.getActiveWorkbenchWindowChecked( event );
    final Shell shell = HandlerUtil.getActiveShellChecked( event );

    final ISelection selection = window.getSelectionService().getSelection( IPageLayout.ID_RES_NAV );

    IResource resource = null;
    if( selection instanceof IStructuredSelection )
    {
      final IStructuredSelection struct = (IStructuredSelection)selection;
      if( struct.size() == 1 )
        resource = (IResource)struct.getFirstElement();
    }

    if( resource == null || !( resource instanceof IFolder ) )
    {
      MessageDialog.openInformation( shell, Messages.getString("SetBasicModelDelegate.0"), //$NON-NLS-1$
          Messages.getString("SetBasicModelDelegate.1") ); //$NON-NLS-1$
      return null;
    }

    final IFolder calcCase = (IFolder)resource;
    final IFile file = calcCase.getFile( ModelNature.CONTROL_NAME );
    if( !file.exists() )
    {
      MessageDialog.openInformation( shell, Messages.getString("SetBasicModelDelegate.2"), //$NON-NLS-1$
          Messages.getString("SetBasicModelDelegate.3") ); //$NON-NLS-1$
      return null;
    }

    final Job job = new Job( Messages.getString("SetBasicModelDelegate.4") + calcCase.getName() ) //$NON-NLS-1$
    {
      /**
       * @see org.eclipse.core.internal.jobs.InternalJob#run(org.eclipse.core.runtime.IProgressMonitor)
       */
      @Override
      protected IStatus run( final IProgressMonitor monitor )
      {
        try
        {
          final ModelNature nature = (ModelNature) calcCase.getProject().getNature( ModelNature.ID );
          return nature.setBasicModel( calcCase, monitor );
        }
        catch( final CoreException e )
        {
          e.printStackTrace();

          return e.getStatus();
        }
      }
    };

    job.addJobChangeListener( new HandleDoneJobChangeAdapter( shell, Messages.getString( "SetBasicModelDelegate.5" ), Messages.getString( "SetBasicModelDelegate.6" ), true, false ) ); //$NON-NLS-1$ //$NON-NLS-2$
    job.setUser( true );
    job.schedule();

    return null;
  }
}
