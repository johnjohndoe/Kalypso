package org.kalypso.util.command;

/**
 * <p>
 * Das Interface zur Implementation eines CommandManagers. Der CommandManager
 * f�hrt eine Liste von Kommandos und koordiniert die Ausf�hrung und den
 * Undo-Mechanismus.
 * </p>
 * <p>
 * Ein Kommando wird mittels {@link ICommandManager#postCommand( ICommand)}dem
 * CommandManager �bergeben und von diesem ausgef�hrt. Das Runnable wird nach dem Kommando zus�tzlich durchgef�hrt
 * </p>
 * Mittels {@link ICommandManager#canUndo()}und
 * {@link ICommandManager#canRedo()}kann ermittelt werden, ob es m�glich ist,
 * das letzt Kommando r�ckg�ngig zu machen oder das zuletzt r�ckg�ngig gemachte
 * wiederherzustellen.
 * <p>
 * Der CommandManager ist ein Publisher, welcher seine Observer �ber die
 * durchgef�hrten Aktionen (postCommand, undo, redo) informiert.
 * </p>
 * 
 * @author von D�mming
 */
public interface ICommandManager
{
  /**
   * F�gt ein Kommando zum Manager hinzu. Ruft {@link ICommand#process()}auf.
   */
  public void postCommand( final ICommand command ) throws Exception;

  public boolean canUndo();

  public void undo() throws Exception;

  public String getUndoDescription();

  public boolean canRedo();

  public void redo() throws Exception;

  public String getRedoDescription();

  public void addCommandManagerListener( final ICommandManagerListener l );

  public void removeCommandManagerListener( final ICommandManagerListener l );
}