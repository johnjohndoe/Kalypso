package org.kalypso.ogc.sensor.commands;

import org.kalypso.ogc.sensor.tableview.swing.ObservationTableModel;
import org.kalypso.util.command.ICommand;

/**
 * AddRowCommand
 * 
 * @author schlienger
 */
public class AddRowCommand implements ICommand
{
  private ObservationTableModel m_model;

  private Object m_value;

  private int m_index;

  /**
   * Constructor
   * @param model
   * @param value
   */
  public AddRowCommand( final ObservationTableModel model, final Object value )
  {
    m_model = model;
    m_value = value;
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
    m_index = m_model.addRow( m_value );
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
    m_model.removeRow( m_index );
  }

  /**
   * @see org.kalypso.util.command.ICommand#getDescription()
   */
  public String getDescription( )
  {
    return "Zeile hinzufügen";
  }
}