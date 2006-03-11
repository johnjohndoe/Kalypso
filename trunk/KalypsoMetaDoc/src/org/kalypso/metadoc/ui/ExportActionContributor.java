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

package org.kalypso.metadoc.ui;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.action.GroupMarker;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IContributionItem;
import org.eclipse.jface.action.IContributionManager;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchPart;
import org.kalypso.metadoc.IExportTarget;
import org.kalypso.metadoc.KalypsoMetaDocPlugin;

/**
 * Helper class to insert all registered export-targets as actions into menubar and toolbar.
 * 
 * @author schlienger
 */
public class ExportActionContributor
{
  /**
   * Fills the export actions into the given target-editor
   * 
   * @param mode
   *          the mode (in the sense of the org.kalypso.metadoc.exportTarget) which denotes which kind of perspective
   *          should be supported by the extension. Optional, can be null, which means take all targets.
   */
  public static ExportAction[] contributeActions( final IEditorPart targetEditor, final String menuPath, final String toolbarGroup, final String mode )
  {
    try
    {
      final IActionBars actionBars = targetEditor.getEditorSite().getActionBars();
      final ExportAction[] actions = createActions( targetEditor, mode );
      final IMenuManager menuManager = actionBars.getMenuManager();

      final IContributionItem menuItem = menuManager.findUsingPath( menuPath );

      final IToolBarManager toolBarManager = actionBars.getToolBarManager();

      for( int i = 0; i < actions.length; i++ )
      {
        final IAction action = actions[i];

        if( menuItem != null )
        {
          if( menuItem instanceof GroupMarker )
          {
            final GroupMarker marker = (GroupMarker) menuItem;
            final IContributionManager parent = marker.getParent();
            parent.appendToGroup( marker.getGroupName(), action );
          }
          else if( menuItem instanceof IMenuManager )
          {
            final IMenuManager mm = (IMenuManager) menuItem;
            mm.add( action );
          }
        }

        if( toolBarManager != null )
          toolBarManager.appendToGroup( toolbarGroup, action );
      }

      return actions;
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
      ErrorDialog.openError( targetEditor.getSite().getShell(), "Export Targets laden", "Fehler beim Laden der Export Targets", e.getStatus() );

      return null;
    }
  }

  private static ExportAction[] createActions( final IWorkbenchPart part, final String mode ) throws CoreException
  {
    final IExportTarget[] targets = KalypsoMetaDocPlugin.getDefault().getTargets();
    final List<ExportAction> actions = new ArrayList<ExportAction>( targets.length );

    for( int i = 0; i < targets.length; i++ )
    {
      final IExportTarget target = targets[i];

      // tricky: only add targets that support our mode
      if( target.isModeSupported( mode ) )
        actions.add( new ExportAction( target, part ) );
    }

    return actions.toArray( new ExportAction[actions.size()] );
  }

}
