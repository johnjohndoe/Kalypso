package org.kalypso.ogc.sensor.tableview.swing.renderer;

import java.awt.Component;
import java.text.DateFormat;

import javax.swing.JLabel;
import javax.swing.JTable;
import javax.swing.table.DefaultTableCellRenderer;


/**
 * Helper: formatiert das Datum auf eine richtige Art und Weise
 * 
 * @author schlienger
 */
public class DateTableCellRenderer extends DefaultTableCellRenderer
{
  // TODO: Wenn die Daten keine Zeit-Information haben, dann wird die aktuelle Systemzeit
  // im TableView angezeit!!!
  private final static DateFormat df = DateFormat.getDateTimeInstance();//new SimpleDateFormat( "dd.MM.yyyy HH:mm:ss" );

  /**
   * @see javax.swing.table.DefaultTableCellRenderer#getTableCellRendererComponent(javax.swing.JTable,
   *      java.lang.Object, boolean, boolean, int, int)
   */
  public Component getTableCellRendererComponent( JTable table, Object value, boolean isSelected,
      boolean hasFocus, int row, int column )
  {
    JLabel label = (JLabel)super.getTableCellRendererComponent( table, value, isSelected,
        hasFocus, row, column );

    label.setText( df.format( value ) );

    return label;
  }
}