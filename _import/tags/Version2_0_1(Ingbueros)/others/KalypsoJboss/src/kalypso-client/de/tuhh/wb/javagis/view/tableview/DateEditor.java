

package de.tuhh.wb.javagis.view.tableview;



import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

import javax.swing.DefaultCellEditor;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JTable;

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

	    private static DateFormat myDateFormat = new SimpleDateFormat("dd.MM.yyyy HH:mm");

	

        public DateEditor(JButton b) {

            super(new JCheckBox()); //Unfortunately, the constructor

                                        //expects a check box, combo box,

                                        //or text field.

            editorComponent = b;

            setClickCountToStart(1); //This is usually 1 or 2.

		    //setUpDateEditor(table);

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
			
			
			System.out.println("getCellEditorValue: "+currentDate.toString());
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
				System.out.println("getTableCellEditorComponent: "+currentDate.toString());
			}

			else

				((JButton)editorComponent).setText("");

            

            return editorComponent;

        }

    }





