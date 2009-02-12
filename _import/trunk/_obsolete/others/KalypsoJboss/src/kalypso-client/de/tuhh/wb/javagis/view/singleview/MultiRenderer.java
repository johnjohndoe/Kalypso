

package de.tuhh.wb.javagis.view.singleview;



import java.awt.Component;

import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JTable;
import javax.swing.table.DefaultTableCellRenderer;

import de.tuhh.wb.javagis.data.TSLink;
import de.tuhh.wb.javagis.view.tableview.DateRenderer;
import de.tuhh.wb.javagis.view.tableview.TableCellRendererBce;

public class MultiRenderer extends DefaultTableCellRenderer {

	

    JCheckBox checkBox = new JCheckBox();

    JLabel label = new JLabel();

    DateRenderer dateRenderer = new DateRenderer();
	
	TableCellRendererBce bceRenderer = new TableCellRendererBce();

    //private DateFormat myDateFormat = new SimpleDateFormat("dd.MM.yyyy HH:mm");

    

    public Component getTableCellRendererComponent(

						   JTable table, Object value,

						   boolean isSelected, boolean hasFocus,

						   int row, int column) {

	if (value instanceof Boolean)

	    {                    // Boolean

		checkBox.setSelected(((Boolean)value).booleanValue());

		checkBox.setHorizontalAlignment(JLabel.CENTER);

		return checkBox;

	    }

	

	if (value instanceof java.util.Date){				//Date

	    return dateRenderer.getTableCellRendererComponent(table,value,isSelected,hasFocus,row,column);

	}
	if (value instanceof TSLink){				//TSLink

	    return bceRenderer.getTableCellRendererComponent(table,value,isSelected,hasFocus,row,column);

	}


	if(value==null)

	    return super.getTableCellRendererComponent(table,"",isSelected,hasFocus,row,column);

	else

	    return super.getTableCellRendererComponent(table,value,isSelected,hasFocus,row,column);

	    //	String str = (value == null) ? "" : value.toString();

    }

}





