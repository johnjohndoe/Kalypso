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
package org.kalypso.util.command;

import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.kalypso.commons.command.ICommandManager;
import org.kalypso.commons.command.ICommandManagerListener;
import org.kalypso.contribs.eclipse.jface.action.FullAction;

/**
 * @author belger
 */
public class UndoRedoAction extends FullAction implements ICommandManagerListener
{
  private ICommandManager m_commandManager;

  private final ISchedulingRule m_rule;

  private final boolean m_isUndo;

  /**
   * @param commandManager
   * @param rule
   * @param bUndo
   *          falls true is die Undo-Action, sonst die Redo-Action
   */
  public UndoRedoAction( final ICommandManager commandManager, final ISchedulingRule rule, final boolean bUndo )
  {
    super( bUndo ? "Undo" : "Redo", null, bUndo ? "letzte Action Rückgängig machen" : "letztes Undo wiederherstellen" );

    m_commandManager = commandManager;
    m_rule = rule;
    m_isUndo = bUndo;

    setCommandManager( commandManager );
  }

  /**
   * @see org.kalypso.commons.command.ICommandManagerListener#onCommandManagerChanged(org.kalypso.commons.command.ICommandManager)
   */
  public void onCommandManagerChanged( final ICommandManager source )
  {
    refresh( source );
  }

  /**
   * @see org.eclipse.jface.action.IAction#run()
   */
  public void run()
  {
    if( ( m_isUndo && m_commandManager.canUndo() ) || ( !m_isUndo && m_commandManager.canRedo() ) )
      new CommandJob( null, m_commandManager, m_rule, null, m_isUndo ? CommandJob.UNDO : CommandJob.REDO );
  }

  public void dispose()
  {
    if( m_commandManager != null )
      m_commandManager.removeCommandManagerListener( this );
  }

  private void refresh( final ICommandManager cm )
  {
    boolean enabled = false;
    String text = "";
    if( cm != null )
    {
      enabled = m_isUndo ? cm.canUndo() : cm.canRedo();
      text = m_isUndo ? ( "Undo: " + cm.getUndoDescription() ) : ( "Redo: " + cm.getRedoDescription() );
    }

    setEnabled( enabled );
    setText( text );
  }

  public void setCommandManager( final ICommandManager manager )
  {
    if( m_commandManager != null )
      m_commandManager.removeCommandManagerListener( this );

    m_commandManager = manager;

    if( m_commandManager != null )
      m_commandManager.addCommandManagerListener( this );

    refresh( m_commandManager );
  }
}