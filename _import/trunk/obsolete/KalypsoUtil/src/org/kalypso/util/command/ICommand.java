package org.kalypso.util.command;

/**
 *
 * @author von D�mming
 */
public interface ICommand
{
    public boolean isUndoable(  );

    public void process(  ) throws Exception;

    public void redo(  ) throws Exception;

    public void undo(  ) throws Exception;
    
    public String getDescription();
}
