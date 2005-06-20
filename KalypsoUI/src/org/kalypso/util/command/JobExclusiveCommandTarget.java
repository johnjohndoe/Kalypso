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
import org.kalypso.eclipse.core.runtime.jobs.MutexSchedulingRule;

/**
 * <p>
 * Implementation eines {@link org.kalypso.util.command.ICommandTarget}
 * </p>
 * <p>
 * Die Kommandos werden via {@link org.kalypso.util.command.CommandJob}abgesetzt
 * </p>
 * 
 * @author gernot
 */
public class JobExclusiveCommandTarget implements ICommandTarget, ICommandManagerListener
{
  /**
   * Jeder Editor hat sein eigenes Mutex, so dass Jobs schön hintereinander ausgeführt werden
   */
  private final ISchedulingRule m_mutexRule = new MutexSchedulingRule();

  private ICommandManager m_commandManager;

  public final UndoRedoAction undoAction;

  public final UndoRedoAction redoAction;

  private final Runnable m_dirtyRunnable;

  public JobExclusiveCommandTarget( final ICommandManager commandManager, final Runnable dirtyRunnable )
  {
    m_dirtyRunnable = dirtyRunnable;
    m_commandManager = commandManager;

    undoAction = new UndoRedoAction( m_commandManager, m_mutexRule, true );
    redoAction = new UndoRedoAction( m_commandManager, m_mutexRule, false );

    setCommandManager( commandManager );
  }

  public void dispose()
  {
    undoAction.dispose();
    redoAction.dispose();

    if( m_commandManager != null )
      m_commandManager.removeCommandManagerListener( this );
  }

  public boolean isDirty()
  {
    return m_commandManager == null ? false : m_commandManager.isDirty();
  }

  public void resetDirty()
  {
    if( m_commandManager != null )
      m_commandManager.resetDirty();
    if( m_dirtyRunnable != null )
      m_dirtyRunnable.run();
  }

  /**
   * @see org.kalypso.util.command.ICommandTarget#postCommand(org.kalypso.util.command.ICommand, java.lang.Runnable)
   */
  public void postCommand( final ICommand command, final Runnable runnable )
  {
    // runnable is unsused!
    new CommandJob( command, m_commandManager, m_mutexRule, m_dirtyRunnable, CommandJob.POST );
  }

  /**
   * @see org.kalypso.util.command.ICommandManagerListener#onCommandManagerChanged(org.kalypso.util.command.ICommandManager)
   */
  public void onCommandManagerChanged( final ICommandManager source )
  {
    if( source != null && source == m_commandManager && source.isDirty() && m_dirtyRunnable != null )
      m_dirtyRunnable.run();
  }

  public ISchedulingRule getSchedulingRule()
  {
    return m_mutexRule;
  }

  public void setCommandManager( final ICommandManager manager )
  {
    if( m_commandManager != null )
      m_commandManager.removeCommandManagerListener( this );

    m_commandManager = manager;

    undoAction.setCommandManager( manager );
    redoAction.setCommandManager( manager );

    if( m_commandManager != null )
      m_commandManager.addCommandManagerListener( this );

    onCommandManagerChanged( manager );
  }
}