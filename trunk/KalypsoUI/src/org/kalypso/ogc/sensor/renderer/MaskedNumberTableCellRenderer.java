package org.kalypso.ogc.sensor.renderer;

import java.awt.Component;
import java.text.NumberFormat;

import javax.swing.JLabel;
import javax.swing.JTable;
import javax.swing.table.DefaultTableCellRenderer;

import org.kalypso.util.jaxbed.RenderingRule;
import org.kalypso.util.jaxbed.Rules;
import org.kalypso.util.status.MaskedNumber;

/**
 * @author schlienger
 *
 */
public class MaskedNumberTableCellRenderer extends DefaultTableCellRenderer
{
  private final static NumberFormat nf = NumberFormat.getInstance();
  private final Rules m_rules;

  public MaskedNumberTableCellRenderer( Rules rules )
  {
    super();
    
    m_rules = rules;
  }

  /**
   * @see javax.swing.table.TableCellRenderer#getTableCellRendererComponent(javax.swing.JTable, java.lang.Object, boolean, boolean, int, int)
   */
  public Component getTableCellRendererComponent( JTable table, Object value, boolean isSelected,
      boolean hasFocus, int row, int column )
  {
    JLabel label = (JLabel)super.getTableCellRendererComponent( table, value, isSelected,
        hasFocus, row, column );

    MaskedNumber mn = (MaskedNumber)value;
    
    if( m_rules != null )
    {
      RenderingRule r = m_rules.findRule( mn.getMask() );

      // TODO check if not null before setting these...
      label.setFont( r.getFont() );
      label.setToolTipText( r.getTooltipText() );
    }

    label.setText( nf.format( mn.doubleValue() ) );

    return label;
  }
}
