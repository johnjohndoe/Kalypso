package org.kalypso.ogc.sensor.commands;

import java.util.Vector;

import org.kalypso.ogc.sensor.tableview.swing.ObservationTableModel;
import org.kalypso.util.command.ICommand;

/**
 * RemoveRowCommand
 * 
 * @author schlienger
 */
public class RemoveRowCommand implements ICommand
{
  private ObservationTableModel m_model;
  private int m_index;
  private Vector m_row;

  /**
   * Constructor
   * 
   * @param model
   * @param index
   */
  public RemoveRowCommand( final ObservationTableModel model, final int index )
  {
    m_model = model;
    m_index = index;
  }

  /**
   * @see org.kalypso.util.command.ICommand#isUndoable()
   */
  public boolean isUndoable( )
  {
    return true;
  }

  /**
   * @see org.kalypso.util.command.ICommand#process()
   */
  public void process( ) throws Exception
  {
    m_row = m_model.removeRow( m_index );
  }

  /**
   * @see org.kalypso.util.command.ICommand#redo()
   */
  public void redo( ) throws Exception
  {
    process();
  }

  /**
   * @see org.kalypso.util.command.ICommand#undo()
   */
  public void undo( ) throws Exception
  {
    m_model.addRow( m_row );
  }

  /**
   * @see org.kalypso.util.command.ICommand#getDescription()
   */
  public String getDescription( )
  {
    return "Entfernt die Zeile " + m_index;
  }
}
