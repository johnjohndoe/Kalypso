package org.kalypso.util.command;


/**
 * <p>Das Interface zur Implementation eines CommandManagers. Der CommandManager führt eine Liste von
 * Kommandos und koordiniert die Ausführung und den Undo-Mechanismus.</p>
 * <p>Ein Kommando wird mittels {@link ICommandManager#postCommand( ICommand )} dem CommandManager übergeben und von diesem
 * ausgeführt.</p>
 * Mittels {@link ICommandManager#canUndo()} und {@link ICommandManager#canRedo()} kann ermittelt werden, ob es möglich ist, das letzt
 * Kommando rückgängig zu machen oder das zuletzt rückgängig gemachte wiederherzustellen.
 * <p>Der CommandManager ist ein Publisher, welcher seine Observer über die durchgeführten Aktionen (postCommand,
 * undo, redo) informiert.</p>
 *
 * @author von Dömming
 */
public interface ICommandManager
{
	/**
	 * Fügt ein Kommando zum Manager hinzu. Ruft {@link ICommand#process()} auf.
     */
    public void postCommand( final ICommand command );

    public boolean canUndo();

    public void undo(  );

    public String getUndoDescription();

    public boolean canRedo();

    public void redo(  );

    public String getRedoDescription();

    public void addCommandManagerListener( final ICommandManagerListener l );

    public void removeCommandManagerListener( final ICommandManagerListener l );
}
