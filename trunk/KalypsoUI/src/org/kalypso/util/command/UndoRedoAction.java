package org.kalypso.util.command;

import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.kalypso.eclipse.jface.action.FullAction;

/**
 * @author belger
 */
public class UndoRedoAction extends FullAction implements ICommandManagerListener
{
  private final ICommandManager m_commandManager;
  private final ISchedulingRule m_rule;
  private final boolean m_isUndo;

  /**
   * @param bUndo falls true is die Undo-Action, sonst die Redo-Action
   */
  public UndoRedoAction( final ICommandManager commandManager, final ISchedulingRule rule, final boolean bUndo )
  {
    super( bUndo ? "Undo" : "Redo", null, bUndo ? "letzte Action Rückgängig machen" : "letztes Undo wiederherstellen" );

    m_commandManager = commandManager;
    m_rule = rule;
    m_isUndo = bUndo;

    m_commandManager.addCommandManagerListener( this );

    refresh( commandManager );
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
    if( ( m_isUndo && m_commandManager.canUndo()) || ( !m_isUndo && m_commandManager.canRedo() ) )
      new CommandJob( null, m_commandManager, m_rule, null, m_isUndo ? CommandJob.UNDO  : CommandJob.REDO );
  }
  
  public void dispose()
  {
    m_commandManager.removeCommandManagerListener( this );
  }

  private void refresh( final ICommandManager cm )
  {
    setEnabled( m_isUndo ? cm.canUndo() : cm.canRedo() );
    
    final String text = m_isUndo ? ( "Undo: " + cm.getUndoDescription() ) 
        : ( "Redo: " + cm.getRedoDescription() );
    setText( text );
  }
}