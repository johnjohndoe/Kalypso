package org.kalypso.util.command;

import java.util.Vector;

import javax.swing.event.EventListenerList;

/**
 * Standardimplementierung von {@link ICommandManager}.

 * @author von D?mming
 */
public class DefaultCommandManager implements ICommandManager
{
  private EventListenerList m_listenerList = new EventListenerList();

  private Vector stack = new Vector();

  private boolean doable = false;

  private boolean undoable = false;

  /** points to last processed command */
  private int stackPos = -1;

  public void postCommand( final ICommand command ) throws Exception
  {
    if( command instanceof InvisibleCommand )
    {
      checkStatus();
      return;
    }
    
      command.process();

      if( command.isUndoable() )
      {
        while( stack.size() - 1 > stackPos )
          stack.removeElementAt( stackPos + 1 );

        stack.add( command );
        stackPos++;
      }
      else
      {
        stack.clear();
        stackPos = -1;
      }

    checkStatus();
  }

  public void redo() throws Exception
  {
    if( stackPos < stack.size() - 1 )
    {
      stackPos++;

      try
      {
        ( (ICommand)stack.elementAt( stackPos ) ).redo();
      }
      catch( final Exception e )
      {
        stackPos--;

        throw e;
      }
    }

    checkStatus();
  }

  public void undo() throws Exception
  {
    if( stackPos >= 0 )
    {
        ( (ICommand)stack.elementAt( stackPos ) ).undo();

        stackPos--;
    }

    checkStatus();
  }

  private void checkStatus()
  {
    undoable = ( stackPos >= 0 );
    doable = ( stackPos < stack.size() - 1 );

    fireCommandManagerChanged();
  }

  public void addCommandManagerListener( final ICommandManagerListener l )
  {
    m_listenerList.add( ICommandManagerListener.class, l );
  }

  public void removeCommandManagerListener( final ICommandManagerListener l )
  {
    m_listenerList.remove( ICommandManagerListener.class, l );
  }

  private void fireCommandManagerChanged()
  {
    final ICommandManagerListener[] listeners = (ICommandManagerListener[])m_listenerList
        .getListeners( ICommandManagerListener.class );
    for( int i = 0; i < listeners.length; i++ )
      listeners[i].onCommandManagerChanged( this );
  }

  /**
   * @see org.kalypso.util.command.ICommandManager#canUndo()
   */
  public boolean canUndo()
  {
    return undoable;
  }

  /**
   * @see org.kalypso.util.command.ICommandManager#canRedo()
   */
  public boolean canRedo()
  {
    return doable;
  }

  /**
   * @see org.kalypso.util.command.ICommandManager#getUndoDescription()
   */
  public String getUndoDescription()
  {
    if( canUndo() )
      return ( (ICommand)stack.elementAt( stackPos ) ).getDescription();

    return "<cannot undo>";
  }

  /**
   * @see org.kalypso.util.command.ICommandManager#getRedoDescription()
   */
  public String getRedoDescription()
  {
    if( canRedo() )
      return ( (ICommand)stack.elementAt( stackPos + 1 ) ).getDescription();

    return "<cannot redo>";
  }

}