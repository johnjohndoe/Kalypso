package org.kalypso.util.command;

/**
 * <p>Das Interface eines Kommandos.</p>
 * <p>Die Methoden {@link #process()}, {@link #redo} und {@link #undo} sollten nie direkt,
 * sondern stets vom {@link ICommandManager} aufgerufen werden.</p>
 *
 * @author von Dömming
 */
public interface ICommand
{
    public boolean isUndoable(  );

    public void process(  ) throws Exception;

    public void redo(  ) throws Exception;

    public void undo(  ) throws Exception;

    public String getDescription();
}
