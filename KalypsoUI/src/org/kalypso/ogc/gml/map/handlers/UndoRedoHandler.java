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

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PlatformUI;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.util.command.CommandJob;

/**
 * Implementation of UndoRedeDelegate (@see org.kalypso.ui.editor.mapeditor.actiondelegates.UndoRedoDelegate.java) as
 * command handler
 * 
 * @author burtscher1
 */
public abstract class UndoRedoHandler extends AbstractHandler
{

  private final boolean m_undo;

  private boolean m_enabled = true;

  public UndoRedoHandler( boolean undo )
  {
    m_undo = undo;
  }

  /**
   * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
   */
  @Override
  public Object execute( ExecutionEvent event ) throws ExecutionException
  {

    final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();
    final IWorkbenchPart part = (IWorkbenchPart) context.getVariable( ISources.ACTIVE_PART_NAME );

    if( part == null )
      return null;

    MapPanel mapPanel = (MapPanel) part.getAdapter( MapPanel.class );

    if( mapPanel == null )
      return null;

    final IMapModell mapModell = mapPanel.getMapModell();
    if( mapModell != null )
    {
      final IKalypsoTheme activeTheme = mapModell.getActiveTheme();
      if( activeTheme instanceof IKalypsoFeatureTheme )
      {
        final IKalypsoFeatureTheme theme = (IKalypsoFeatureTheme) activeTheme;

        final CommandableWorkspace workspace = theme.getWorkspace();

        if( (m_undo && workspace.canUndo()) || (!m_undo && workspace.canRedo()) )
          new CommandJob( null, workspace, theme.getSchedulingRule(), null, m_undo ? CommandJob.UNDO : CommandJob.REDO );
      }
    }

    refreshAction();
    return null;
  }

  /**
   * Right now, this mehod always returns true - which means, that the handler is enabled even if the workspace has no
   * un- or redoable actions; problem: the refreshEnablement gets called only when the active part is switched TODO: Set
   * context if something is undoable
   */
  @Override
  public boolean isEnabled( )
  {
    // return m_enabled;
    return true;
  }

  /**
   * adapted from {@link org.kalypso.ui.editor.mapeditor.actiondelegates.UndoRedoDelegate#refreshAction()} and should be
   * replaced by handling enablement through setting a context
   */
  protected void refreshAction( )
  {

    IWorkbenchPart activePart = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().getActivePart();
    if( activePart == null )
      return;
    final MapPanel mapPanel = (MapPanel) activePart.getAdapter( MapPanel.class );

    if( mapPanel == null )
      return;

    boolean bEnabled = false;

    final IMapModell mapModell = mapPanel.getMapModell();
    if( mapModell != null )
    {
      final IKalypsoTheme activeTheme = mapModell.getActiveTheme();
      if( activeTheme != null && activeTheme instanceof IKalypsoFeatureTheme )
      {
        final IKalypsoFeatureTheme theme = (IKalypsoFeatureTheme) activeTheme;
        final CommandableWorkspace workspace = theme.getWorkspace();
        if( workspace != null )
          bEnabled = m_undo ? workspace.canUndo() : workspace.canRedo();
      }
    }
    m_enabled = bEnabled;
  }

}
