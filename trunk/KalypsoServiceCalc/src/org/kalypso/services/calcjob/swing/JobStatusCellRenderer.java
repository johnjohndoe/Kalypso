package org.kalypso.services.calcjob.swing;

import java.awt.Component;

import javax.swing.JTable;
import javax.swing.table.DefaultTableCellRenderer;


/**
 *
 * @author Belger
 */
public class JobStatusCellRenderer extends DefaultTableCellRenderer
{
  public Component getTableCellRendererComponent( 
    JTable table,
    Object value,
    boolean isSelected,
    boolean hasFocus,
    int row,
    int column
   )
  {
    super.getTableCellRendererComponent( table, value, isSelected, hasFocus, row, column );

    setToolTipText( getText() );

    return this;
  }
}
