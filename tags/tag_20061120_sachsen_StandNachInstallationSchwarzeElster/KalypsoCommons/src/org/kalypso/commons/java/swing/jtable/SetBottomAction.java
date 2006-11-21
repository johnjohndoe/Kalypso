package org.kalypso.commons.java.swing.jtable;

import java.awt.event.ActionEvent;

import javax.swing.JTable;

/**
 * SetAllAction
 * 
 * @author schlienger
 */
public class SetBottomAction extends AbstractObservationTableAction
{
  public SetBottomAction( JTable table )
  {
    super( table, "Werte unterhalb setzen", "Führt den selektierten Wert für alle nachfolgenden fort" );
  }

  public void internalActionPerformed( ActionEvent e )
  {
    final JTable table = getTable();
    final int col = table.getSelectedColumn();
    final int row = table.getSelectedRow();
    final Object value = table.getValueAt( row, col );

    for( int i = row + 1; i < table.getRowCount(); i++ )
      table.setValueAt( value, i, col );
  }
}