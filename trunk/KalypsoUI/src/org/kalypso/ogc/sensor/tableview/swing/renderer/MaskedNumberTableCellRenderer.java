package org.kalypso.ogc.sensor.tableview.swing.renderer;

import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import java.text.NumberFormat;

import javax.swing.JLabel;
import javax.swing.JTable;
import javax.swing.table.DefaultTableCellRenderer;

import org.kalypso.ogc.sensor.tableview.rules.RenderingRule;
import org.kalypso.ogc.sensor.tableview.swing.ObservationTableModel;

/**
 * @author schlienger
 *  
 */
public class MaskedNumberTableCellRenderer extends DefaultTableCellRenderer
{
  private final static NumberFormat nf = NumberFormat.getInstance();

  /**
   * @see javax.swing.table.TableCellRenderer#getTableCellRendererComponent(javax.swing.JTable,
   *      java.lang.Object, boolean, boolean, int, int)
   */
  public Component getTableCellRendererComponent( JTable table, Object value,
      boolean isSelected, boolean hasFocus, int row, int column )
  {
    final JLabel label = (JLabel) super.getTableCellRendererComponent( table,
        value, isSelected, hasFocus, row, column );

    final Number n = (Number) value;

    if( n == null )
      return label;

    final ObservationTableModel otm = (ObservationTableModel) table.getModel();
    final RenderingRule[] r = otm.findRules( row, column );

    String ttext = "";

    for( int i = 0; i < r.length; i++ )
    {
      final Font f = r[i].getFont();
      if( f != null )
        label.setFont( f );

      ttext += r[i].getTooltipText();

      final Color fgc = r[i].getForegroundColor();
      if( fgc != null )
        label.setForeground( fgc );

      final Color bgc = r[i].getBackgroundColor();
      if( bgc != null )
        label.setBackground( bgc );
    }

    label.setToolTipText( ttext );

    label.setText( nf.format( n.doubleValue() ) );

    return label;
  }
}