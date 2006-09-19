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
package org.kalypso.ui.editor.gistableeditor.actions;

import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.WorkspaceModifyOperation;
import org.eclipse.ui.progress.IProgressService;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.table.LayerTableViewer;
import org.kalypso.ui.editor.AbstractGisEditorActionDelegate;
import org.kalypso.ui.editor.gistableeditor.GisTableEditor;
import org.kalypso.ui.editor.mapeditor.actiondelegates.WidgetActionPart;

/**
 * @author belger
 */
public class SaveThemeDelegate extends AbstractGisEditorActionDelegate
{
  /**
   * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  public void run( final IAction action )
  {
    final WidgetActionPart part = getPart();
    if( part == null )
      return;

    // WARNING: Because of the following cast, we can only use
    // this delegate with the GisTableEditor.
    final GisTableEditor editor = (GisTableEditor) part.getPart();
    if( editor == null )
      return;

    final Shell shell = editor.getSite().getShell();
    if( !MessageDialog.openConfirm( shell, "Themen speichern", "Sollen die Daten des aktiven Themas gespeichert werden?" ) )
      return;

    final IKalypsoFeatureTheme theme = editor.getLayerTable().getTheme();
    if( theme != null )
    {
      final IProgressService progressService = PlatformUI.getWorkbench().getProgressService();

      final LayerTableViewer layerTable = editor.getLayerTable();

      final WorkspaceModifyOperation op = new WorkspaceModifyOperation()
      {
        @Override
        protected void execute( final IProgressMonitor monitor ) throws CoreException
        {
          layerTable.saveData( monitor );
        }
      };

      try
      {
        progressService.busyCursorWhile( op );
      }
      catch( final InvocationTargetException e )
      {
        e.printStackTrace();

        final CoreException ce = (CoreException) e.getTargetException();
        ErrorDialog.openError( shell, "Fehler", "Fehler beim Speichern", ce.getStatus() );
      }
      catch( final InterruptedException e )
      {
        e.printStackTrace();
      }
    }
    refreshAction( action, getSelection() );
  }

  @Override
  protected void refreshAction( final IAction action, final ISelection selection )
  {
    boolean enabled = false;

    final WidgetActionPart part = getPart();
    if( part == null )
      return;

    // WARNING: Because of the following cast, we can only use
    // this delegate with the GisTableEditor.
    final GisTableEditor editor = (GisTableEditor) part.getPart();
    if( editor != null )
    {
      final IKalypsoFeatureTheme theme = editor.getLayerTable().getTheme();
      if( theme != null )
      {
        final CommandableWorkspace workspace = theme.getWorkspace();
        if( workspace != null )
          enabled = workspace.isDirty();
      }
    }

    action.setEnabled( enabled );
  }
}