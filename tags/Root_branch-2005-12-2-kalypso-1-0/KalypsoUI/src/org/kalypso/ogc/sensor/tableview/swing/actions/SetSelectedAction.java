package org.kalypso.ogc.sensor.tableview.swing.actions;

import java.awt.event.ActionEvent;

import org.kalypso.ogc.sensor.tableview.swing.ObservationTable;

/**
 * SetAllAction
 * 
 * @author schlienger
 */
public class SetSelectedAction extends AbstractObservationTableAction
{
  public SetSelectedAction( ObservationTable table )
  {
    super( table, "Selektierte Werte setzen", "Setzt die selektierten Werte auf den aktiven Wert" );
  }

  public void internalActionPerformed( ActionEvent e )
  {
    final ObservationTable table = getTable();
    final int col = table.getSelectedColumn();
    final int row = table.getSelectedRow();
    final int[] rows = table.getSelectedRows();
    final Object value = table.getValueAt( row, col );

    for( int i = 0; i < rows.length; i++ )
      table.setValueAt( value, rows[i], col );
  }
}