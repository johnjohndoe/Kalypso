package org.kalypso.editor.mapeditor.commands;

import org.eclipse.swt.widgets.Shell;
import org.kalypso.editor.mapeditor.Propsdiag;
import org.kalypso.util.command.ICommand;
import org.kalypso.xml.types.LayerType;

/**
 * @author belger
 */
public class EditPropertiesCommand implements ICommand
{
  private final Shell m_shell;
  private final LayerType m_layer;

  public EditPropertiesCommand( final Shell shell, final LayerType layer)
  {
    m_layer = layer;
    m_shell = shell;
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
    new Propsdiag( m_shell, m_layer ).open();
  }

  /**
   * @see org.kalypso.util.command.ICommand#redo()
   */
  public void redo() throws Exception
  {
    // lässt sich nicht undoen
  }

  /**
   * @see org.kalypso.util.command.ICommand#undo()
   */
  public void undo() throws Exception
  {
    // lässt sich nicht undoen  
  }

  /**
   * @see org.kalypso.util.command.ICommand#getDescription()
   */
  public String getDescription()
  {
    return "Themeneigenschaften editieren";
  }
}
