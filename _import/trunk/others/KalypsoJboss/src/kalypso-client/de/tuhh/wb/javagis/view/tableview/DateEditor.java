
package de.tuhh.wb.javagis.view.tableview;

import javax.swing.DefaultCellEditor;
import javax.swing.JLabel;
import java.util.Date;
import javax.swing.JTable;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import java.awt.*;
import java.awt.event.*;

import java.util.Calendar;
import java.util.TimeZone;
import java.util.Locale;

import de.tuhh.wb.javagis.jcalendar.src.com.toedter.calendar.JCalendar;
import de.tuhh.wb.javagis.jcalendar.src.com.toedter.calendar.JMonthChooser;
import de.tuhh.wb.javagis.jcalendar.src.com.toedter.calendar.JCalendarField;
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
		
		/**private static void setUpDateEditor(JTable table) {
		
        //First, set up the button that brings up the dialog.
		final JButton button = new JButton("");
        button.setBackground(Color.white);
        button.setBorderPainted(false);
        button.setMargin(new Insets(0,0,0,0));

        //Now create an editor to encapsulate the button, and
        //set it up as the editor for all Date cells.
		final DateEditor dateEditor = new DateEditor(button);
        table.setDefaultEditor(Date.class, dateEditor);

        //Set up the dialog that the button brings up.
		final JCalendar calendar = new JCalendar(JMonthChooser.NO_SPINNER);
		calendar.setLocale(Locale.getDefault());
		
        ActionListener okListener = new ActionListener() {
            public void actionPerformed(ActionEvent e) {
			Calendar currentCalendar=calendar.getCalendar();
            dateEditor.currentDate = currentCalendar.getTime();
			//dateEditor.currentDate.setHours(currentCalendar.get(Calendar.HOUR_OF_DAY));
		    dateEditor.stopCellEditing();
				/*		int row = table.getEditingRow();
			int col = table.getEditingColumn();
			myModel.setValueAt(dateEditor.currentDate,row,col);
			table.tableChanged(new TableModelEvent(myModel,row,row,col));
				 */
		/**	    System.out.println("currentDate: "+dateEditor.currentDate.toString());
            }
        };
		
		ActionListener cancelListener = new ActionListener() {
            public void actionPerformed(ActionEvent e) {
				dateEditor.cancelCellEditing();
			}
		};
		
		final JDialog dialog = JCalendar.createDialog(button,
                                        "",
                                        true,
                                        calendar,
                                        okListener,
		                             cancelListener); //XXXDoublecheck this is OK
		dialog.addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent e) {
				dateEditor.cancelCellEditing();
                dialog.hide();
            }
		});
        //Here's the code that brings up the dialog.
        button.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
				
				Calendar myCalendar = Calendar.getInstance();
				if(dateEditor.currentDate!=null)
			{
				button.setText(myDateFormat.format(dateEditor.currentDate));
				myCalendar.set(dateEditor.currentDate.getYear()+1900,dateEditor.currentDate.getMonth(),dateEditor.currentDate.getDate(),dateEditor.currentDate.getHours(),dateEditor.currentDate.getMinutes());
			}
			else
				button.setText("");
			
                calendar.setCalendar(myCalendar);
				//System.out.println("Year:"+dateEditor.currentDate.getYear()+1900+"myCalender:"+myCalendar.toString());
                //Without the following line, the dialog comes up
                //in the middle of the screen.
                dialog.setLocationRelativeTo(button);
                dialog.show();
				dialog.toFront();
            }
        });
		 }*/
	
    }


