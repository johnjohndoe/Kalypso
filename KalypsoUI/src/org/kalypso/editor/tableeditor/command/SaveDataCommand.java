package org.kalypso.editor.tableeditor.command;

import org.kalypso.editor.tableeditor.GisTableEditor;
import org.kalypso.util.command.ICommand;

/**
 * @author gernot
 */
public class SaveDataCommand implements ICommand
{

  private final GisTableEditor m_editor;

  public SaveDataCommand( final GisTableEditor editor )
  {
    m_editor = editor;
  }

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
    m_editor.saveData();
  }

  /**
   * @see org.kalypso.util.command.ICommand#redo()
   */
  public void redo() throws Exception
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see org.kalypso.util.command.ICommand#undo()
   */
  public void undo() throws Exception
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see org.kalypso.util.command.ICommand#getDescription()
   */
  public String getDescription()
  {
    return "Daten speichern";
  }

}
