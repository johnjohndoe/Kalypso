package org.kalypso.editor.tableeditor.layerTable.command;

import org.deegree.model.feature.Feature;
import org.kalypso.editor.tableeditor.layerTable.LayerTableModel;
import org.kalypso.util.command.ICommand;

/**
 * @author bce
 */
public class RemoveRowsCommand implements ICommand
{
  private final LayerTableModel m_model;
  private Feature[] m_features;

  public RemoveRowsCommand( final LayerTableModel model, final Feature[] features )
  {
    m_model = model;
    m_features = features;
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
      m_model.removeFeature(m_features[i]);  
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
      m_model.addFeature( m_features[i] );  
  }

  /**
   * @see org.kalypso.util.command.ICommand#getDescription()
   */
  public String getDescription()
  {
    return "Element löschen";
  }
}
