package org.kalypso.commons.java.swing.jtable;

import java.awt.event.ActionEvent;

import javax.swing.JTable;

/**
 * SetAllAction
 * 
 * @author schlienger
 */
public class SetTopAction extends AbstractObservationTableAction
{
  public SetTopAction( JTable table )
  {
    super( table, "Werte oberhalb setzen", "Führt den selektierten Wert für alle vorhergehenden fort" );
  }

  public void internalActionPerformed( ActionEvent e )
  {
    final JTable table = getTable();
    final int col = table.getSelectedColumn();
    final int row = table.getSelectedRow();
    final Object value = table.getValueAt( row, col );

    for( int i = 0; i < row; i++ )
      table.setValueAt( value, i, col );
  }
}