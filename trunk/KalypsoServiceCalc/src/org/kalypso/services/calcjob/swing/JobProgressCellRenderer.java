package org.kalypso.services.calcjob.swing;

import java.awt.Color;
import java.awt.Component;

import javax.swing.JLabel;
import javax.swing.JProgressBar;
import javax.swing.JTable;
import javax.swing.SwingConstants;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.TableCellRenderer;

import org.kalypso.services.calcjob.CalcJobStatus;

/**
 * @author Donald Duck
 */
public class JobProgressCellRenderer implements TableCellRenderer
{
  private JProgressBar m_progressBar = new JProgressBar( SwingConstants.HORIZONTAL, 0, 100 );
  private DefaultTableCellRenderer m_defaultRenderer = new DefaultTableCellRenderer();   
  
  public JobProgressCellRenderer( )
  {
    m_progressBar.setBorderPainted( false );
    m_progressBar.setStringPainted( true );
  }

  public Component getTableCellRendererComponent(
    JTable table,
    Object value,
    boolean isSelected,
    boolean hasFocus,
    int row,
    int column)
  {
    final CalcJobStatus js = ((JobQueueTableModel)table.getModel()).getState( row );
  
    final Color c = isSelected ? js.color.darker() : js.color.brighter(); 
    
    if( js == CalcJobStatus.RUNNING_STATE )
    {
      if( isSelected )
        m_progressBar.setBackground( table.getSelectionBackground() );
      else 
        m_progressBar.setBackground( table.getBackground() );
      
      m_progressBar.setForeground( c );
      m_progressBar.setValue( ((Integer)value).intValue() );

      return m_progressBar;
    }    
  
    final JLabel defaultComponent = (JLabel)m_defaultRenderer.getTableCellRendererComponent( table, value, isSelected, hasFocus, row, column );
    defaultComponent.setText( js.name );
    defaultComponent.setToolTipText( js.name );
    defaultComponent.setBackground( c );
    return defaultComponent;
  }

}
