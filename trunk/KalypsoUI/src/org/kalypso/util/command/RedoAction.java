package org.kalypso.util.command;

import org.kalypso.eclipse.jface.action.FullAction;

/**
 * @author belger
 */
public class RedoAction extends FullAction implements ICommandManagerListener
{
  private final ICommandManager m_commandManager;

  public RedoAction( final ICommandManager commandManager )
  {
    super( "Redo", null, "letztes Undo wiederherstellen" );

    m_commandManager = commandManager;

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
    if( m_commandManager.canRedo() )
      m_commandManager.redo();
  }
  
  public void dispose()
  {
    m_commandManager.removeCommandManagerListener( this );
  }

  private void refresh( ICommandManager cm )
  {
    setEnabled( cm.canRedo() );
    setText( "Redo: " + cm.getRedoDescription() );
  }
}