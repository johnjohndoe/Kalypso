package org.kalypso.util.command;

import org.kalypso.eclipse.jface.action.FullAction;

/**
 * @author belger
 */
public class UndoAction extends FullAction implements ICommandManagerListener
{
  private final ICommandManager m_commandManager;

  public UndoAction( final ICommandManager commandManager )
  {
    super( "Undo", null, "letzte Action R?ckg?ngig machen" );

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
    if( m_commandManager.canUndo() )
    {
      try
      {
        m_commandManager.undo();
      }
      catch( Exception e )
      {
        e.printStackTrace();
      }
    }
  }
  
  public void dispose()
  {
    m_commandManager.removeCommandManagerListener( this );
  }

  private void refresh( ICommandManager cm )
  {
    setEnabled( cm.canUndo() );
    setText( "Undo: " + cm.getUndoDescription() );
  }
}