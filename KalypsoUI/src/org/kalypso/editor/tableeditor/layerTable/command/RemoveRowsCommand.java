package org.kalypso.editor.tableeditor.layerTable.command;

import org.kalypso.editor.tableeditor.layerTable.LayerTableModel;
import org.kalypso.ogc.gml.KalypsoFeature;
import org.kalypso.util.command.ICommand;

/**
 * @author bce
 */
public class RemoveRowsCommand implements ICommand
{
  private final LayerTableModel m_model;
  private KalypsoFeature[] m_features;

  public RemoveRowsCommand( final LayerTableModel model, final KalypsoFeature[] features )
  {
    m_model = model;
    m_features=features;
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
    for( int i = 0; i < m_features.length; i++ )
      m_model.removeRow(m_features[i]);  
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
    for( int i = 0; i < m_features.length; i++ )
      m_model.addRow( m_features[i] );  
  }

  /**
   * @see org.kalypso.util.command.ICommand#getDescription()
   */
  public String getDescription()
  {
    return "Element l?schen";
  }
}
