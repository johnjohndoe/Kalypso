package org.kalypso.ogc.sensor.tableview.swing.renderer;

import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import java.text.NumberFormat;

import javax.swing.Icon;
import javax.swing.JLabel;
import javax.swing.JTable;
import javax.swing.SwingConstants;
import javax.swing.table.DefaultTableCellRenderer;

import org.kalypso.ogc.sensor.tableview.rules.RenderingRule;
import org.kalypso.ogc.sensor.tableview.swing.ObservationTableModel;

/**
 * Handles the rendering with the given NumberFormat and for each
 * value asks the ObservationTableModel for possible RenderingRules
 * that will be used to modify the layout: icon, tooltip, color, etc.
 * 
 * @author schlienger
 */
public class MaskedNumberTableCellRenderer extends DefaultTableCellRenderer
{
  private final NumberFormat m_nf;

  public MaskedNumberTableCellRenderer( final NumberFormat nf )
  {
    setHorizontalAlignment( SwingConstants.RIGHT );
    m_nf = nf;
  }
  
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

    final RenderingRule[] r = ((ObservationTableModel) table.getModel())
        .findRules( row, column );

    // reset visual settings
    label.setToolTipText( null );
    label.setForeground( null );
    label.setBackground( null );
    label.setIcon( null );
    label.setFont( null );

    // different handling when cell is selected/focus
    if( isSelected )
    {
      if( hasFocus )
        label.setBackground( Color.LIGHT_GRAY );
      else
        label.setBackground( Color.YELLOW );
    }
    
    // apply rendering rule
    String ttext = table.getColumnName( column );
    for( int i = 0; i < r.length; i++ )
    {
      // TOOLTIP
      ttext += "; " + r[i].getTooltipText();

      // FONT
      final Font f = r[i].getFont();
      label.setFont( f );

      final Icon ic = r[i].getIcon();
      label.setIcon( ic );
      
      if( !isSelected )
      {
        // FOREGROUND
        final Color fgc = r[i].getForegroundColor();
        label.setForeground( fgc );

        // BACKGROUND
        final Color bgc = r[i].getBackgroundColor();
        label.setBackground( bgc );
      }
    }

    label.setToolTipText( ttext );
    
    label.setText( m_nf.format( n.doubleValue() ) );
    return label;
  }
}