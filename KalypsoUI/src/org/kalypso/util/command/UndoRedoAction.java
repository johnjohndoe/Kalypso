package org.kalypso.util.command;

import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.kalypso.eclipse.jface.action.FullAction;

/**
 * @author belger
 */
public class UndoRedoAction extends FullAction implements ICommandManagerListener
{
  private ICommandManager m_commandManager;

  private final ISchedulingRule m_rule;

  private final boolean m_isUndo;

  /**
   * @param bUndo
   *          falls true is die Undo-Action, sonst die Redo-Action
   */
  public UndoRedoAction( final ICommandManager commandManager, final ISchedulingRule rule,
      final boolean bUndo )
  {
    super( bUndo ? "Undo" : "Redo", null, bUndo ? "letzte Action Rückgängig machen"
        : "letztes Undo wiederherstellen" );

    m_commandManager = commandManager;
    m_rule = rule;
    m_isUndo = bUndo;

    setCommandManager( commandManager );
  }

  /**
   * @see org.kalypso.util.command.ICommandManagerListener#onCommandManagerChanged(org.kalypso.util.command.ICommandManager)
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
      new CommandJob( null, m_commandManager, m_rule, null, m_isUndo ? CommandJob.UNDO
          : CommandJob.REDO );
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
      text = m_isUndo ? ( "Undo: " + cm.getUndoDescription() ) : ( "Redo: " + cm
          .getRedoDescription() );
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