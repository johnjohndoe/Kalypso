package de.tuhh.wb.javagis.view.tableview;



import java.awt.Component;

import javax.swing.JLabel;
import javax.swing.JTable;
import javax.swing.table.TableCellRenderer;




public class TableCellRendererBce extends JLabel implements TableCellRenderer

{


    public TableCellRendererBce()

    {
		super();
    }

    

    public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int col)

    {
	if(value!=null)

			 setText(value.toString());

			else

			 setText("");

	return this;

    }

}

