package org.kalypso.editor.tableeditor.actions;

import org.deegree.model.feature.Feature;
import org.kalypso.editor.tableeditor.layerTable.LayerTableModel;
import org.kalypso.util.command.ICommand;

/**
 * @author bce
 */
public class AddRowCommand implements ICommand
{
  private final LayerTableModel m_model;
  private Feature m_feature;

  public AddRowCommand( final LayerTableModel model, final Feature feature )
  {
    m_model = model;
    m_feature = feature;
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
    m_model.addFeature( m_feature );
  }

  /**
   * @see org.kalypso.util.command.ICommand#redo()
   */
  public void redo() throws Exception
  {
    m_model.addFeature ( m_feature );  
  }

  /**
   * @see org.kalypso.util.command.ICommand#undo()
   */
  public void undo() throws Exception
  {
    m_model.removeFeature( m_feature );  
  }

  /**
   * @see org.kalypso.util.command.ICommand#getDescription()
   */
  public String getDescription()
  {
    return "Element hinzufügen";
  }
}
