package org.kalypso.ogc.sensor.tableview.swing.actions;

import java.awt.event.ActionEvent;

import org.kalypso.i18n.Messages;
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
    super( table, Messages.getString("org.kalypso.ogc.sensor.tableview.swing.actions.SetSelectedAction.0"), Messages.getString("org.kalypso.ogc.sensor.tableview.swing.actions.SetSelectedAction.1") ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  @Override
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