package org.kalypso.util.command;

/**
 * <p>Eine Art Dummy-Kommando, welches dazu dient, vom ICommandManager einfach geschluckt zu werden.</p>
 * <p>Es wird zwar ausgeführt, aber nicht in die Commandqueue aufgenommen</p>
 * 
 * 
 * @author Belger
 */
public class InvisibleCommand implements ICommand
{
  /**
   * @see org.kalypso.util.command.ICommand#isUndoable()
   */
  public boolean isUndoable()
  {
    return false;
  }

  /**
   * @see org.kalypso.util.command.ICommand#process()
   */
  public void process() throws Exception
  {
    // nichts tun
  }

  /**
   * @see org.kalypso.util.command.ICommand#redo()
   */
  public void redo() throws Exception
  {
    process();
  }

  /**
   * @see org.kalypso.util.command.ICommand#undo()
   */
  public void undo() throws Exception
  {
    // nichts tun
  }

  /**
   * @see org.kalypso.util.command.ICommand#getDescription()
   */
  public String getDescription()
  {
    return null;
  }

}
