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
package org.kalypso.ui.navigator;

import java.lang.reflect.InvocationTargetException;
import java.util.Vector;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IViewActionDelegate;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.WorkspaceModifyOperation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.diagview.DiagViewUtils;
import org.kalypso.ogc.sensor.diagview.grafik.GrafikLauncher;

/**
 * Opens the Grafik tool. Can operate on observation template and grafik
 * template files.
 * 
 * @author schlienger
 */
public class GrafikViewActionDelegate implements IViewActionDelegate
{
  private IFile m_currentFile = null;

  private IViewPart m_view;

  public GrafikViewActionDelegate( )
  {
    // empty
  }

  /**
   * @see org.eclipse.ui.IViewActionDelegate#init(org.eclipse.ui.IViewPart)
   */
  public void init( final IViewPart view )
  {
    m_view = view;
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  public void run( final IAction action )
  {
    final IFile currentFile = m_currentFile;
    if( currentFile == null )
      return;

    final WorkspaceModifyOperation operation = new WorkspaceModifyOperation(
        null )
    {
      protected void execute( IProgressMonitor monitor ) throws CoreException, InvocationTargetException
      {
        monitor.beginTask( "Grafik öffnen", IProgressMonitor.UNKNOWN );
        try
        {
          if( currentFile.getFileExtension().equalsIgnoreCase(
              DiagViewUtils.ODT_FILE_EXTENSION ) )
          {
            final IContainer parent = currentFile.getParent();

            final IFolder folder = parent.getFolder( new Path( "grafik" ) );

            GrafikLauncher.startGrafikODT( currentFile, folder, monitor );
          }
          else if( currentFile.getFileExtension().equalsIgnoreCase(
              GrafikLauncher.TPL_FILE_EXTENSION ) )
          {
            GrafikLauncher.startGrafikTPL( currentFile, new Vector() );
          }
        }
        catch( SensorException e )
        {
          // TODO ich verstehe nicht warum, aber die exceptions die hier gethrown werden, werden
          // nicht richtig in der GUI dargestellt. Eclipse zeit einen Dialogbox mit leerem
          // Message. Das ist schlecht! Siehe mit Marc.
          e.printStackTrace();
          throw new InvocationTargetException( e );
        }
        finally
        {
          monitor.done();
        }
      }
    };

    try
    {
      PlatformUI.getWorkbench().getProgressService()
          .busyCursorWhile( operation );
    }
    catch( Exception e )
    {
      MessageDialog.openError( m_view.getSite().getShell(),
          "Grafik konnte nicht gestartet werden", e.getLocalizedMessage() );
    }
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
   *      org.eclipse.jface.viewers.ISelection)
   */
  public void selectionChanged( final IAction action, final ISelection selection )
  {
    final IStructuredSelection sel = (IStructuredSelection) selection;

    if( sel.getFirstElement() instanceof IFile )
      m_currentFile = (IFile) sel.getFirstElement();
    else
      m_currentFile = null;
  }
}