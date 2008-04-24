package org.kalypso.ogc.sensor.tableview.swing.actions;

import java.awt.event.ActionEvent;

import org.kalypso.ogc.sensor.tableview.swing.ObservationTable;

/**
 * SetAllAction
 * 
 * @author schlienger
 */
public class SetTopAction extends AbstractObservationTableAction
{
  public SetTopAction( ObservationTable table )
  {
    super( table, "Werte oberhalb setzen", "Führt den selektierten Wert für alle vorhergehenden fort" );
  }

  @Override
  public void internalActionPerformed( ActionEvent e )
  {
    final ObservationTable table = getTable();
    final int col = table.getSelectedColumn();
    final int row = table.getSelectedRow();
    final Object value = table.getValueAt( row, col );

    for( int i = 0; i < row; i++ )
      table.setValueAt( value, i, col );
  }
}