package org.kalypso.util.command;



/**
 * @author von Dömming
 */
public interface ICommandManager
{
    public void postCommand( ICommand command );

    public void redo(  );

    public void undo(  );
    
    public boolean canUndo();
    
    public boolean canRedo();
    
    public String getUndoText();
    
    public String getRedoText();

    public void addCommandManagerListener( final ICommandManagerListener l );
    
    public void removeCommandManagerListener( final ICommandManagerListener l );
}
