package org.kalypso.editor.tableeditor.layerTable.command;

import org.kalypso.editor.tableeditor.layerTable.LayerTableModel;
import org.kalypso.ogc.sort.DisplayContext;
import org.kalypso.util.command.ICommand;

/**
 * @author bce
 */
public class RemoveRowsCommand implements ICommand
{
  private final LayerTableModel m_model;
  private DisplayContext[] m_displayContexts;

  public RemoveRowsCommand( final LayerTableModel model, final DisplayContext[] displayContexts )
  {
    m_model = model;
    m_displayContexts = displayContexts;
  }

  /**
   * @see org.kalypso.util.command.ICommand#isUndoable()
   */
  public boolean isUndoable()
  {
    return true;
  }

  /**
   * @see org.kalypso.util.command.ICommand#process()
   */
  public void process() throws Exception
  {
    for( int i = 0; i < m_displayContexts.length; i++ )
      m_model.removeRow(m_displayContexts[i]);  
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
    for( int i = 0; i < m_displayContexts.length; i++ )
      m_model.addRow( m_displayContexts[i] );  
  }

  /**
   * @see org.kalypso.util.command.ICommand#getDescription()
   */
  public String getDescription()
  {
    return "Element l?schen";
  }
}
