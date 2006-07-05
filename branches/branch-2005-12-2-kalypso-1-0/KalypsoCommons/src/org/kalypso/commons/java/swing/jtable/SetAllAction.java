package org.kalypso.commons.java.swing.jtable;

import java.awt.event.ActionEvent;

import javax.swing.JTable;

/**
 * SetAllAction
 * 
 * @author schlienger
 */
public class SetAllAction extends AbstractObservationTableAction
{
  public SetAllAction( JTable table )
  {
    super( table, "Alle Werte setzen", "Setzt alle Werte der Spalte auf den selektierten Wert" );
  }

  public void internalActionPerformed( ActionEvent e )
  {
    final JTable table = getTable();
    final int col = table.getSelectedColumn();
    final int row = table.getSelectedRow();
    final Object value = table.getValueAt( row, col );

    for( int i = 0; i < row; i++ )
      table.setValueAt( value, i, col );

    for( int i = row + 1; i < table.getRowCount(); i++ )
      table.setValueAt( value, i, col );
  }
}
