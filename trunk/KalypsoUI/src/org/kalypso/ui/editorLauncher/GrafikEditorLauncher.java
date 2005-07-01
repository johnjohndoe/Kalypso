/*--------------- Kalypso-Header ------------------------------------------

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

 --------------------------------------------------------------------------*/

package org.kalypso.ui.editorLauncher;

import java.lang.reflect.InvocationTargetException;
import java.util.Vector;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.ui.IEditorLauncher;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.WorkspaceModifyOperation;
import org.eclipse.ui.internal.Workbench;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.diagview.DiagViewUtils;
import org.kalypso.ogc.sensor.diagview.grafik.GrafikLauncher;
import org.kalypso.ui.KalypsoGisPlugin;

/**
 * @author schlienger
 */
public class GrafikEditorLauncher implements IEditorLauncher
{
  /**
   * @see org.eclipse.ui.IEditorLauncher#open(org.eclipse.core.runtime.IPath)
   */
  public void open( final IPath path )
  {
    final IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
    final IFile file = root.getFileForLocation( path );

    final WorkspaceModifyOperation operation = new WorkspaceModifyOperation( null )
    {
      protected void execute( IProgressMonitor monitor ) throws InvocationTargetException
      {
        IStatus status = Status.OK_STATUS;

        monitor.beginTask( "Grafik öffnen", IProgressMonitor.UNKNOWN );
        try
        {
          final IContainer parent = file.getParent();

          final IFolder folder = parent.getFolder( new Path( "grafik" ) );

          if( path.getFileExtension().equalsIgnoreCase( DiagViewUtils.ODT_FILE_EXTENSION ) )
            status = GrafikLauncher.startGrafikODT( file, folder, monitor );
          else if( file.getFileExtension().equalsIgnoreCase( GrafikLauncher.TPL_FILE_EXTENSION ) )
            status = GrafikLauncher.startGrafikTPL( file, new Vector() );
          else if( file.getFileExtension().equalsIgnoreCase( "zml" ) )
            status = GrafikLauncher.startGrafikZML( file, folder, monitor );
          else
            status = KalypsoGisPlugin.createErrorStatus( "Datei-typ nicht erkannt für Grafikprogramm", null );
        }
        catch( final SensorException e )
        {
          status = KalypsoGisPlugin.createErrorStatus( "Fehler beim Start von Grafik aufgetreten", e );
        }
        finally
        {
          monitor.done();

          if( !status.isOK() )
            ErrorDialog.openError( Workbench.getInstance().getActiveWorkbenchWindow().getShell(), "Grafik öffnen",
                "Siehe Details", status );
        }
      }
    };

    try
    {
      PlatformUI.getWorkbench().getProgressService().busyCursorWhile( operation );
    }
    catch( final Exception e )
    {
      MessageDialog.openError( Workbench.getInstance().getActiveWorkbenchWindow().getShell(),
          "Grafik konnte nicht gestartet werden", e.getLocalizedMessage() );
    }
  }
}
