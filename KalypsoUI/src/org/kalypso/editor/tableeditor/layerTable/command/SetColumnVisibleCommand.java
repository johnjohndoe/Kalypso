package org.kalypso.editor.tableeditor.layerTable.command;

import org.deegree.model.feature.FeatureTypeProperty;
import org.kalypso.editor.tableeditor.layerTable.LayerTableModel;
import org.kalypso.util.command.ICommand;

/**
 * @author bce
 */
public class SetColumnVisibleCommand implements ICommand
{
  private final LayerTableModel m_model;
  private final FeatureTypeProperty m_ftp;
  private final boolean m_bVisible;

  public SetColumnVisibleCommand( final LayerTableModel model, final FeatureTypeProperty ftp, final boolean bVisible )
  {
    m_model = model;
    m_ftp = ftp;
    m_bVisible = bVisible;
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
    m_model.showColumn( m_ftp, m_bVisible );
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
    m_model.showColumn( m_ftp, !m_bVisible );
  }

  /**
   * @see org.kalypso.util.command.ICommand#getDescription()
   */
  public String getDescription()
  {
    return "Spalte '" + m_ftp.getName() + "' " + ( m_bVisible ? "anzeigen" : "verstecken" );
  }

}
