package org.kalypso.util.command;

import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.kalypso.eclipse.core.runtime.jobs.MutexSchedulingRule;

/**
 * <p>
 * Implementation eines {@link org.kalypso.util.command.ICommandTarget}
 * </p>
 * <p>
 * Die Kommandos werden via {@link org.kalypso.util.command.CommandJob}
 * abgesetzt
 * </p>
 * 
 * @author gernot
 */
public class JobExclusiveCommandTarget implements ICommandTarget, ICommandManagerListener
{
  /**
   * Jeder Editor hat sein eigenes Mutex, so dass Jobs schön hintereinander
   * ausgeführt werden
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
    m_commandManager.resetDirty();
  }

  /**
   * @see org.kalypso.util.command.ICommandTarget#postCommand(org.kalypso.util.command.ICommand,
   *      java.lang.Runnable)
   */
  public void postCommand( ICommand command, Runnable runnable )
  {
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