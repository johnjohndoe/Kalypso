package view;

import java.awt.Color;
import java.awt.Component;
import java.io.File;

import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.ListCellRenderer;

/**
 * ListCellRenderer for JComboBox with files as values(objects), displays only
 * the filename of the file in JComboBox
 * 
 * @author N. Peiler
 *  
 */
class GridCellRenderer extends JLabel implements ListCellRenderer
{
  public GridCellRenderer()
  {
    setOpaque( true );
  }

  public Component getListCellRendererComponent( JList list, Object value, int index,
      boolean isSelected, boolean cellHasFocus )
  {
    if( value != null )
    {
      File gridFile = (File)value;
      setText( gridFile.getName() );
      setBackground( isSelected ? Color.red : Color.white );
      setForeground( isSelected ? Color.white : Color.black );
    }
    else
    {
      setText( "" );
      setBackground( isSelected ? Color.red : Color.white );
      setForeground( isSelected ? Color.white : Color.black );
    }
    return this;

  }
}