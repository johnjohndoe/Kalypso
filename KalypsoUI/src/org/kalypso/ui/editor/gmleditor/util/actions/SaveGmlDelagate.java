/*----------------    FILE HEADER KALYPSO ------------------------------------------
*
*  This file is part of kalypso.
*  Copyright (C) 2004 by:
* 
*  Technical University Hamburg-Harburg (TUHH)
*  Institute of River and coastal engineering
*  Denickestraﬂe 22
*  21073 Hamburg, Germany
*  http://www.tuhh.de/wb
* 
*  and
*  
*  Bjoernsen Consulting Engineers (BCE)
*  Maria Trost 3
*  56070 Koblenz, Germany
*  http://www.bjoernsen.de
* 
*  This library is free software; you can redistribute it and/or
*  modify it under the terms of the GNU Lesser General Public
*  License as published by the Free Software Foundation; either
*  version 2.1 of the License, or (at your option) any later version.
* 
*  This library is distributed in the hope that it will be useful,
*  but WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*  Lesser General Public License for more details.
* 
*  You should have received a copy of the GNU Lesser General Public
*  License along with this library; if not, write to the Free Software
*  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
* 
*  Contact:
* 
*  E-Mail:
*  belger@bjoernsen.de
*  schlienger@bjoernsen.de
*  v.doemming@tuhh.de
*   
*  ---------------------------------------------------------------------------*/
package org.kalypso.ui.editor.gmleditor.util.actions;

import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.WorkspaceModifyOperation;
import org.eclipse.ui.progress.IProgressService;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.editor.AbstractGisEditorActionDelegate;
import org.kalypso.ui.editor.gmleditor.ui.GmlEditor;
import org.kalypso.ui.editor.gmleditor.ui.GmlTreeView;
import org.kalypso.ui.editor.mapeditor.actiondelegates.WidgetActionPart;

public class SaveGmlDelagate extends AbstractGisEditorActionDelegate
{
  /**
   * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  public void run( final IAction action )
  {
    final WidgetActionPart part = getPart();
    if( part == null)
      return;

    // WARNING: Because of the following cast, we can only use
    // this delegate with the GmlEditor.
    final GmlEditor editor = (GmlEditor) part.getPart();
    if( editor == null )
      return;

    final Shell shell = part.getSite().getShell();
    if( !MessageDialog.openConfirm( shell, "Themen speichern", "Sollen die Daten des Baums gespeichert werden?" ) )
      return;
    
    final GmlTreeView treeViewer = editor.getTreeView();
    if( treeViewer != null )
    {
      final IProgressService progressService = PlatformUI.getWorkbench().getProgressService();

      final WorkspaceModifyOperation op = new WorkspaceModifyOperation()
      {
        @Override
        protected void execute( final IProgressMonitor monitor ) throws CoreException
        {
          treeViewer.saveData( monitor );
          refreshAction( action );
        }
      };

      try
      {
        progressService.busyCursorWhile( op );
      }
      catch( final InvocationTargetException e )
      {
        e.printStackTrace();

        final CoreException ce = (CoreException)e.getTargetException();
        ErrorDialog.openError( shell, "Fehler", "Fehler beim Speichern", ce.getStatus() );
      }
      catch( final InterruptedException e )
      {
        e.printStackTrace();
      }
    }

  }

  @Override
  protected void refreshAction( IAction action )
  {
    boolean bEnabled = false;

    final WidgetActionPart part = getPart();
    if( part == null )
      return;

    // WARNING: Because of the following cast, we can only use
    // this delegate with the GmlEditor.
    final GmlEditor editor = (GmlEditor) part.getPart();
    if( editor != null )
    {
      final GmlTreeView treeViewer = editor.getTreeView();
      if( treeViewer != null )
      {
        final CommandableWorkspace workspace = treeViewer.getWorkspace();
        if( workspace != null )
          bEnabled = workspace.isDirty();
      }
    }
    if( action != null )
      getAction().setEnabled( bEnabled );
  }

}