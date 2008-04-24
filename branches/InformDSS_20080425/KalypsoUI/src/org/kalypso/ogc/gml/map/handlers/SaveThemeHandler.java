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
package org.kalypso.ogc.gml.map.handlers;

import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.WorkspaceModifyOperation;
import org.eclipse.ui.progress.IProgressService;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoSaveableTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ui.editor.mapeditor.AbstractMapPart;

/**
 * @author burtscher1
 */
public class SaveThemeHandler extends AbstractHandler
{

  private boolean m_isEnabled = true;

  /**
   * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
   */
  @SuppressWarnings("unused")
  @Override
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();
    final IWorkbenchPart part = (IWorkbenchPart) context.getVariable( ISources.ACTIVE_PART_NAME );

    if( part == null )
      return null;

    final MapPanel mapPanel = (MapPanel) part.getAdapter( MapPanel.class );

    if( mapPanel == null )
      return null;

    final Shell shell = part.getSite().getShell();

    if( mapPanel != null )
    {
      final IMapModell mapModell = mapPanel.getMapModell();
      if( mapModell != null )
      {
        final IKalypsoTheme activeTheme = mapModell.getActiveTheme();
        if( activeTheme instanceof IKalypsoSaveableTheme )
        {

          if( (activeTheme != null) && (activeTheme instanceof IKalypsoFeatureTheme) )
          {
            final IKalypsoFeatureTheme fTheme = (IKalypsoFeatureTheme) activeTheme;
            final CommandableWorkspace workspace = fTheme.getWorkspace();
            // only save if map is dirty
            if( workspace != null && workspace.isDirty() )
            {
              if( !MessageDialog.openConfirm( shell, "Themen speichern", "Sollen die Daten des aktiven Themas gespeichert werden?" ) )
                return null;

              final IKalypsoSaveableTheme theme = (IKalypsoSaveableTheme) activeTheme;

              final IProgressService progressService = PlatformUI.getWorkbench().getProgressService();

              final WorkspaceModifyOperation op = new WorkspaceModifyOperation()
              {
                @Override
                protected void execute( final IProgressMonitor monitor ) throws CoreException
                {
                  final IWorkbenchPart workbenchPart = part;
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
      }
    }
    refreshEnablement();
    return null;
  }

  /**
   * TODO: Right now, this mehod always returns true - which means, that the handler is enabled even if the workspace
   * isn't dirty; problem: the refreshEnablement gets called only when the active part is switched
   */
  @Override
  public boolean isEnabled( )
  {
    // return m_isEnabled
    return true;

  }

  /**
   * adapted from {@link org.kalypso.ui.editor.mapeditor.actiondelegates.SaveThemeDelegate#refreshAction()} and should
   * be replaced by handling enablement through setting a context
   */
  private void refreshEnablement( )
  {
    boolean bEnabled = false;

    IWorkbenchPart activePart = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().getActivePart();
    final MapPanel mapPanel = (MapPanel) activePart.getAdapter( MapPanel.class );

    if( mapPanel == null )
      return;

    final IMapModell mapModell = mapPanel.getMapModell();
    if( mapModell != null )
    {
      final IKalypsoTheme activeTheme = mapModell.getActiveTheme();

      if( (activeTheme != null) && (activeTheme instanceof IKalypsoFeatureTheme) )
      {
        final IKalypsoFeatureTheme fTheme = (IKalypsoFeatureTheme) activeTheme;
        final CommandableWorkspace workspace = fTheme.getWorkspace();
        if( workspace != null && workspace.isDirty() )
        {
          bEnabled = true;
        }
      }

    }
    m_isEnabled = bEnabled;
  }

}
