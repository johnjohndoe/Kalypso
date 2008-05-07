package org.kalypso.ogc.sensor.tableview.swing.actions;

import java.awt.event.ActionEvent;

import org.kalypso.i18n.Messages;
import org.kalypso.ogc.sensor.tableview.swing.ObservationTable;

/**
 * SetAllAction
 * 
 * @author schlienger
 */
public class SetBottomAction extends AbstractObservationTableAction
{
  public SetBottomAction( ObservationTable table )
  {
    super( table, Messages.getString("org.kalypso.ogc.sensor.tableview.swing.actions.SetBottomAction.0"), Messages.getString("org.kalypso.ogc.sensor.tableview.swing.actions.SetBottomAction.1") ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  @Override
  public void internalActionPerformed( ActionEvent e )
  {
    final ObservationTable table = getTable();
    final int col = table.getSelectedColumn();
    final int row = table.getSelectedRow();
    final Object value = table.getValueAt( row, col );

    for( int i = row + 1; i < table.getRowCount(); i++ )
      table.setValueAt( value, i, col );
  }
}