package org.kalypso.ogc.sensor.tableview.renderer;

import java.awt.Component;
import java.text.NumberFormat;

import javax.swing.JLabel;
import javax.swing.JTable;
import javax.swing.table.DefaultTableCellRenderer;


public class NumberTableCellRenderer extends DefaultTableCellRenderer
{
  private final static NumberFormat nf = NumberFormat.getInstance();

  /**
   * @see javax.swing.table.DefaultTableCellRenderer#getTableCellRendererComponent(javax.swing.JTable,
   *      java.lang.Object, boolean, boolean, int, int)
   */
  public Component getTableCellRendererComponent( JTable table, Object value, boolean isSelected,
      boolean hasFocus, int row, int column )
  {
    JLabel label = (JLabel)super.getTableCellRendererComponent( table, value, isSelected,
        hasFocus, row, column );

    label.setText( nf.format( value ) );

    return label;
  }
}
