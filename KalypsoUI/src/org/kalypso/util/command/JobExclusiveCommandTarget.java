package org.kalypso.util.command;

import org.eclipse.core.runtime.jobs.ISchedulingRule;

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
   * Jeder Editor hat sein eigenes Mutex, so dass Jobs sch?n hintereinander
   * ausgef?hrt werden
   */
  private final Mutex m_mutexRule = new Mutex();

  private final ICommandManager m_commandManager = new DefaultCommandManager();

  public final UndoRedoAction undoAction = new UndoRedoAction( m_commandManager, m_mutexRule, true );

  public final UndoRedoAction redoAction = new UndoRedoAction( m_commandManager, m_mutexRule, false );

  private boolean m_isDirty = false;

  private final Runnable m_dirtyRunnable;

  public JobExclusiveCommandTarget( final Runnable dirtyRunnable )
  {
    m_dirtyRunnable = dirtyRunnable;

    m_commandManager.addCommandManagerListener( this );
  }

  public void dispose()
  {
    undoAction.dispose();
    redoAction.dispose();

    m_commandManager.removeCommandManagerListener( this );
  }

  /**
   * @see org.eclipse.ui.part.EditorPart#isDirty()
   */
  public boolean isDirty()
  {
    return m_isDirty;
  }

  public void setDirty( final boolean dirty )
  {
    m_isDirty = dirty;

    if( m_dirtyRunnable != null )
      m_dirtyRunnable.run();
  }

  /**
   * @see org.kalypso.util.command.ICommandTarget#postCommand(org.kalypso.util.command.ICommand,
   *      java.lang.Runnable)
   */
  public void postCommand( ICommand command, Runnable runnable )
  {
    new CommandJob( command, m_commandManager, m_mutexRule, null, CommandJob.POST );
  }

  /**
   * @see org.kalypso.util.command.ICommandManagerListener#onCommandManagerChanged(org.kalypso.util.command.ICommandManager)
   */
  public void onCommandManagerChanged( final ICommandManager source )
  {
    if( source == m_commandManager )
      setDirty( true );
  }
  
  /**
   * <p>
   * Diese Regel sichert, dass alle Kommandos die über dieses Target abgesetzt
   * werden, nacheinander ausgeführt werden.
   * </p>
   * <p>
   * Sollte nicht statisch sein, da sonst alle Kommandos aller solcher Targets
   * nacheinander abgesetzt werden.
   * </p>
   */
  private final class Mutex implements ISchedulingRule
  {
    public boolean isConflicting( final ISchedulingRule rule )
    {
      return rule == this;
    }

    public boolean contains( final ISchedulingRule rule )
    {
      return rule == this;
    }
  }
}