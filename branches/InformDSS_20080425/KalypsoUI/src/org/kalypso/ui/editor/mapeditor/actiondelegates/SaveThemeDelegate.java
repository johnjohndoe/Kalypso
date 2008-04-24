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
package org.kalypso.ui.editor.mapeditor.actiondelegates;

import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.WorkspaceModifyOperation;
import org.eclipse.ui.progress.IProgressService;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.IKalypsoSaveableTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ui.editor.AbstractGisEditorActionDelegate;
import org.kalypso.ui.editor.mapeditor.AbstractMapPart;
import org.kalypsodeegree.model.feature.event.ModellEventListener;

/**
 * @author belger
 */
public class SaveThemeDelegate extends AbstractGisEditorActionDelegate implements ModellEventListener
{
  /**
   * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  public void run( final IAction action )
  {
    final WidgetActionPart part = getPart();
    if( part == null )
      return;

    final Shell shell = part.getSite().getShell();
    if( !MessageDialog.openConfirm( shell, "Themen speichern", "Sollen die Daten des aktiven Themas gespeichert werden?" ) )
      return;

    final MapPanel mapPanel = part.getMapPanel();
    if( mapPanel != null )
    {
      final IMapModell mapModell = mapPanel.getMapModell();
      if( mapModell != null )
      {
        final IKalypsoTheme activeTheme = mapModell.getActiveTheme();
        if( activeTheme instanceof IKalypsoSaveableTheme )
        {
          final IKalypsoSaveableTheme theme = (IKalypsoSaveableTheme) activeTheme;

          final IProgressService progressService = PlatformUI.getWorkbench().getProgressService();

          final WorkspaceModifyOperation op = new WorkspaceModifyOperation()
          {
            @Override
            protected void execute( final IProgressMonitor monitor ) throws CoreException
            {
              final IWorkbenchPart workbenchPart = part.getPart();
              if( workbenchPart == null )
                return;
              else if( workbenchPart instanceof AbstractMapPart )
                theme.saveFeatures( monitor );
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
      }
    }

    refreshAction( action, getSelection() );
  }

  @Override
  protected void refreshAction( final IAction action, final ISelection selection )
  {
    boolean bEnabled = false;

    final WidgetActionPart part = getPart();
    if( part != null )
    {
      final AbstractMapPart editor = (AbstractMapPart) part.getPart();
      if( editor != null )
      {
        final IMapModell mapModell = editor.getMapPanel().getMapModell();
        if( mapModell != null )
        {
          final IKalypsoTheme activeTheme = mapModell.getActiveTheme();
          if( (activeTheme != null) && (activeTheme instanceof IKalypsoFeatureTheme) )
          {
            final IKalypsoFeatureTheme theme = (IKalypsoFeatureTheme) activeTheme;
            final CommandableWorkspace workspace = theme.getWorkspace();
            if( workspace != null )
              bEnabled = workspace.isDirty();
          }
        }
      }
    }

    action.setEnabled( bEnabled );
  }
}