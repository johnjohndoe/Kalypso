package org.kalypso.ogc.sensor.tableview.swing.actions;

import java.awt.event.ActionEvent;
import java.util.Arrays;

import org.kalypso.ogc.sensor.tableview.swing.ObservationTable;

/**
 * SetAllAction
 * 
 * @author schlienger
 */
public class InterpolateSelectedAction extends AbstractObservationTableAction
{
  public InterpolateSelectedAction( ObservationTable table )
  {
    super(
        table,
        "Werte interpolieren",
        "Interpoliert die Werte der Spalte zwischen dem ersten und dem letzten selektierten Wert" );
  }

  public void internalActionPerformed( ActionEvent e )
  {
    final ObservationTable table = getTable();
    final int col = table.getSelectedColumn();
    final int[] rows = table.getSelectedRows();
        
    if( rows.length == 0 )
      return;
    
    Arrays.sort( rows );
    final int minRow = rows[0];
    final int maxRow = rows[rows.length - 1];
    
    final Object obj1 = table.getValueAt( minRow, col );
    final Object obj2 = table.getValueAt( maxRow, col );
    
    if( !(obj1 instanceof Number) || !(obj2 instanceof Number) )
      return;
    
    final double v1 = ((Number) obj1).doubleValue();
    final double v2 = ((Number) obj2).doubleValue();

    final double step = (v2 - v1) / (maxRow - minRow);
    
    int i = 1;
    for( int row = minRow + 1; row < maxRow; row++ )
    {
      final Double value = new Double( v1 + i * step );
      table.setValueAt( value, row, col );
      
      i++;
    }
  }
}