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
    super( table, "Selektierte Werte setzen", "Setzt Werte der Spalte auf den selektierten Wert, welche selektiert sind" );
  }

  /**
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  public void actionPerformed( ActionEvent e )
  {
    final ObservationTable table = getTable();
    final int col = table.getSelectedColumn();
    final int row = table.getSelectedRow();
    final int[] rows = table.getSelectedRows();
    final Object value = table.getValueAt(row, col);

    for( int i = 0; i < rows.length; i++ )
      table.setValueAt( value, rows[i], col );
    
    table.repaint();
  }
}
