package org.kalypso.editor.tableeditor.layerTable.command;

import org.kalypso.editor.tableeditor.layerTable.LayerTableModel;
import org.kalypso.ogc.gml.KalypsoFeature;
import org.kalypso.util.command.ICommand;

/**
 * @author bce
 */
public class AddRowCommand implements ICommand
{
  private final LayerTableModel m_model;
  private final KalypsoFeature m_feature;

  public AddRowCommand( final LayerTableModel model, final KalypsoFeature fe)
  {
    m_model = model;
    m_feature = fe;
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
    m_model.addRow(m_feature );
  }

  /**
   * @see org.kalypso.util.command.ICommand#redo()
   */
  public void redo() throws Exception
  {
    m_model.addRow ( m_feature );  
  }

  /**
   * @see org.kalypso.util.command.ICommand#undo()
   */
  public void undo() throws Exception
  {
    m_model.removeRow( m_feature );  
  }

  /**
   * @see org.kalypso.util.command.ICommand#getDescription()
   */
  public String getDescription()
  {
    return "Element hinzuf?gen";
  }
}
