
package de.tuhh.wb.javagis.view.tableview;

import java.awt.Component;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

import javax.swing.JLabel;
import javax.swing.JTable;
import javax.swing.table.TableCellRenderer;

public class DateRenderer extends JLabel
                        implements TableCellRenderer {
	
	private DateFormat myDateFormat = new SimpleDateFormat("dd.MM.yyyy HH:mm");
	
        public DateRenderer() {
            super();
			
        }

		public Component getTableCellRendererComponent(
                                JTable table, Object date,
                                boolean isSelected, boolean hasFocus,
                                int row, int column) {
			//try{
			//DateFormat dateFormat = new SimpleDateFormat("yyyy.MM.dd HH.mm");
			//Date newDate = DateFormat.getInstance().parse((String)date);
			if(date!=null)
			 setText(myDateFormat.format((Date)date));
			else
			 setText("");
			//}
			    //catch(ParseException e)
			//{
			    //System.out.println("wrong DateFormat, couldn't parse");
			 //}
			//setText(DateFormat.getInstance().format(date));
            return this;
        }
    }


