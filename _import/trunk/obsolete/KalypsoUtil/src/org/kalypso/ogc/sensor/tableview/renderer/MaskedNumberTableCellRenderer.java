package org.kalypso.ogc.sensor.tableview.renderer;

import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import java.text.NumberFormat;

import javax.swing.JLabel;
import javax.swing.JTable;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.TableModel;

import org.kalypso.ogc.sensor.tableview.swing.ObservationTableModel;
import org.kalypso.ogc.sensor.tableview.template.RenderingRule;
import org.kalypso.util.status.MaskedNumber;

/**
 * @author schlienger
 *
 */
public class MaskedNumberTableCellRenderer extends DefaultTableCellRenderer
{
  private final static NumberFormat nf = NumberFormat.getInstance();
  
  /**
   * @see javax.swing.table.TableCellRenderer#getTableCellRendererComponent(javax.swing.JTable, java.lang.Object, boolean, boolean, int, int)
   */
  public Component getTableCellRendererComponent( JTable table, Object value, boolean isSelected,
      boolean hasFocus, int row, int column )
  {
    final JLabel label = (JLabel)super.getTableCellRendererComponent( table, value, isSelected,
        hasFocus, row, column );

    final MaskedNumber mn = (MaskedNumber)value;
    
    TableModel tm = table.getModel();
    if( tm instanceof ObservationTableModel )
    {
      final RenderingRule r = ((ObservationTableModel)tm).getRules().findRule( mn.getMask() );

      if( r != null )
      {
        final Font f = r.getFont();
        if( f != null )
          label.setFont( f );
        
        label.setToolTipText( r.getTooltipText() );
        
        final Color fgc = r.getForegroundColor();
        if( fgc != null )
          label.setForeground( fgc );
        
        final Color bgc = r.getBackgroundColor();
        if( bgc != null )
          label.setBackground( bgc );
      }
    }

    label.setText( nf.format( mn.doubleValue() ) );

    return label;
  }
}
