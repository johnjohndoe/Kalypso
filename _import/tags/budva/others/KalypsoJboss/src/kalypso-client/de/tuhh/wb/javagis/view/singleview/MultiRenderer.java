

package de.tuhh.wb.javagis.view.singleview;



import java.awt.*;

import java.awt.event.*;

import javax.swing.*;

import javax.swing.table.*;

import javax.swing.event.*;

import java.text.DateFormat;

import java.util.Date;

import java.text.SimpleDateFormat;

import de.tuhh.wb.javagis.view.tableview.DateRenderer;

import de.tuhh.wb.javagis.view.tableview.TableCellRendererBce;

import de.tuhh.wb.javagis.data.TSLink;

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





