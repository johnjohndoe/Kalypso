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
package org.kalypso.ui.action;

import java.io.File;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.window.Window;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.eclipse.ui.dialogs.ListSelectionDialog;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.util.synchronize.ModelSynchronizer;

/**
 * @author belger
 */
public class GetCalcCaseDelegate implements IWorkbenchWindowActionDelegate
{
  private IWorkbenchWindow m_window;

  /**
   * @see org.eclipse.ui.IWorkbenchWindowActionDelegate#dispose()
   */
  public void dispose()
  {
  //  
  }

  /**
   * @see org.eclipse.ui.IWorkbenchWindowActionDelegate#init(org.eclipse.ui.IWorkbenchWindow)
   */
  public void init( final IWorkbenchWindow window )
  {
    m_window = window;
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  public void run( final IAction action )
  {
    try
    {
      final File serverRoot = ModelActionHelper.getServerRoot();

      final IProject project = ModelActionHelper.chooseOneProject( m_window );

      final File serverProject = ModelActionHelper.checkIsSeverMirrored( serverRoot, project );

      final ModelSynchronizer synchronizer = new ModelSynchronizer( project, serverProject );
      final File[] remoteCalcCases = synchronizer.getRemoteCalcCases();

      final String serverPath = serverProject.getAbsolutePath();
      final int serverPathLength = serverPath.length();
      final String[] remotePathes = new String[remoteCalcCases.length];
      final Map pathHash = new HashMap();
      for( int i = 0; i < remoteCalcCases.length; i++ )
      {
        final File calcDir = remoteCalcCases[i];
        final String calcPath = calcDir.getAbsolutePath();
        if( calcPath.length() > serverPathLength + 1 )
        {
          final String calcPathRel = calcPath.substring( serverPathLength + 1 );
          remotePathes[i] = calcPathRel;
          pathHash.put( calcPathRel, calcDir );
        }
      }

      final ListSelectionDialog dialog = new ListSelectionDialog(
          m_window.getShell(),
          remotePathes,
          new ArrayContentProvider(),
          new LabelProvider(),
          "Wählen Sie die Rechenvarianten, die vom Server geladen werden.\n"
              + "Ist im Arbeitsbereich bereits an gleicher Stelle eine Rechenvariante vorhanden, wird diese überschrieben." );
      if( dialog.open() != Window.OK )
        return;

      final Object[] result = dialog.getResult();

      final Job job = new Job( "Rechenvarianten vom Server laden" )
      {
        protected IStatus run( final IProgressMonitor monitor )
        {
          monitor.beginTask( "Rechenvarianten vom Server laden", 1000 * result.length );

          final Collection errorStati = new LinkedList();

          for( int i = 0; i < result.length; i++ )
          {
            final String relPath = result[i].toString();
            final File calcDir = (File)pathHash.get( relPath );

            try
            {
              synchronizer.getFolder( calcDir, relPath, new SubProgressMonitor( monitor, 1000 ) );
            }
            catch( final CoreException e )
            {
              errorStati.add( e.getStatus() );
              e.printStackTrace();
            }
          }

          if( errorStati.isEmpty() )
            return Status.OK_STATUS;

          final IStatus[] stati = (IStatus[])errorStati.toArray( new IStatus[errorStati.size()] );
          return new MultiStatus( KalypsoGisPlugin.getId(), 0, stati,
              "Nicht alle Rechenvarianten konnten geladen werden.", null );
        }
      };

      job.setUser(true);
      job.schedule();
    }
    catch( final CoreException ce )
    {
      ErrorDialog.openError( m_window.getShell(), "Rechvarianten vom Server laden",
          "Rechenvarianten konnten nicht vom Server geladen werden.", ce.getStatus() );
    }
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
   *      org.eclipse.jface.viewers.ISelection)
   */
  public void selectionChanged( IAction action, ISelection selection )
  {
  //
  }

}