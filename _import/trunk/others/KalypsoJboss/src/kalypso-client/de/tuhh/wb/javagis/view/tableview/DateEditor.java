
package de.tuhh.wb.javagis.view.tableview;

import javax.swing.DefaultCellEditor;
import javax.swing.JLabel;
import java.util.Date;
import javax.swing.JTable;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import java.awt.*;
import java.awt.event.*;
 /*
     * The editor button that brings up the dialog.
     * We extend DefaultCellEditor for convenience,
     * even though it mean we have to create a dummy
     * check box.  Another approach would be to copy
     * the implementation of TableCellEditor methods
     * from the source code for DefaultCellEditor.
     */
    public class DateEditor extends DefaultCellEditor {
        Date currentDate = null;
	    private DateFormat myDateFormat = new SimpleDateFormat("dd.MM.yyyy HH:mm");
        public DateEditor(JButton b) {
                super(new JCheckBox()); //Unfortunately, the constructor
                                        //expects a check box, combo box,
                                        //or text field.
            editorComponent = b;
            setClickCountToStart(1); //This is usually 1 or 2.

            //Must do this so that editing stops when appropriate.
            b.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    //fireEditingStopped();
                }
            });
        }

        protected void fireEditingStopped() {
            super.fireEditingStopped();
        }

        public Object getCellEditorValue() {
            return currentDate;
        }

        public Component getTableCellEditorComponent(JTable table,
                                                     Object value,
                                                     boolean isSelected,
                                                     int row,
                                                     int column) {
			if(value!=null)
			{
				((JButton)editorComponent).setText(myDateFormat.format(value));
				currentDate = (Date)value;
			}
			else
				((JButton)editorComponent).setText("");
            
            return editorComponent;
        }
    }


