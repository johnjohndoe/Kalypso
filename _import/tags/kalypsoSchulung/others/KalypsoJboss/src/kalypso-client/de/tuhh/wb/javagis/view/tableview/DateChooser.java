package de.tuhh.wb.javagis.view.tableview;

import javax.swing.JTable;
import javax.swing.table.AbstractTableModel;
import javax.swing.DefaultCellEditor;
import javax.swing.table.TableCellRenderer;
import javax.swing.event.*;

import javax.swing.JLabel;
import javax.swing.JDialog;
import javax.swing.JButton;
import javax.swing.JCheckBox;

import javax.swing.JScrollPane;
import javax.swing.JOptionPane;
import javax.swing.JFrame;
import javax.swing.SwingUtilities;
import java.awt.*;
import java.awt.event.*;
import java.util.Date;
import java.text.ParseException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.TimeZone;
import java.util.Locale;

import de.tuhh.wb.javagis.jcalendar.src.com.toedter.calendar.JCalendar;
import de.tuhh.wb.javagis.jcalendar.src.com.toedter.calendar.JMonthChooser;
import de.tuhh.wb.javagis.jcalendar.src.com.toedter.calendar.JCalendarField;

public class DateChooser
{

    private boolean DEBUG = false;
    public static DateFormat myDateFormat = new SimpleDateFormat("dd.MM.yyyy HH:mm");
    public DateChooser()
	{
	}
	
	public static void setUpDateChooser(JTable table)
		{
        //Set up renderer and editor for the Favorite Date column.
        setUpDateRenderer(table);
        setUpDateEditor(table);

	}
    
    private static void setUpDateRenderer(JTable table) {
        table.setDefaultRenderer(Date.class,
                                 new DateRenderer());
    }

    private static void setUpDateEditor(JTable table) {
		
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
		    System.out.println("currentDate: "+dateEditor.currentDate.toString());
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
    }

	
    static class DateRenderer extends JLabel
                        implements TableCellRenderer {

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

	
	
    /*
     * The editor button that brings up the dialog.
     * We extend DefaultCellEditor for convenience,
     * even though it mean we have to create a dummy
     * check box.  Another approach would be to copy
     * the implementation of TableCellEditor methods
     * from the source code for DefaultCellEditor.
     */
    static class DateEditor extends DefaultCellEditor {
        Date currentDate = null;

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

    /*class MyTableModel extends AbstractTableModel {
        final String[] columnNames = {"Number",
                                      "Date"};
        final Object[][] data = {
            {new Integer (1), new Date(102,5,8,15,3)},
            {new Integer (2), new Date(103,6,6,20,16)},
        };

        public int getColumnCount() {
            return columnNames.length;
        }
        
        public int getRowCount() {
            return data.length;
        }

        public String getColumnName(int col) {
            return columnNames[col];
        }

        public Object getValueAt(int row, int col) {
            return data[row][col];
        }

        /*
         * JTable uses this method to determine the default renderer/
         * editor for each cell.  If we didn't implement this method,
         * then the last column would contain text ("true"/"false"),
         * rather than a check box.
         */
	/*public Class getColumnClass(int c) {
            return getValueAt(0, c).getClass();
        }

        /*
         * Don't need to implement this method unless your table's
         * editable.
         */
	/*public boolean isCellEditable(int row, int col) {
            //Note that the data/cell address is constant,
            //no matter where the cell appears onscreen.
			/* if (col < 1) {
                return false;
            } else {
                return true;
			 }*/
	/*		return true;
        }

        public void setValueAt(Object value, int row, int col) {
//            if (DEBUG) {
                System.out.println("Setting value at " + row + "," + col
                                   + " to " + value
                                   + " (an instance of "
                                   + value.getClass() + ")");
            //}

            data[row][col] = value;
            fireTableCellUpdated(row, col);

            if (DEBUG) {
                System.out.println("New value of data:");
                printDebugData();
            }
        }

        private void printDebugData() {
            int numRows = getRowCount();
            int numCols = getColumnCount();

            for (int i=0; i < numRows; i++) {
                System.out.print("    row " + i + ":");
                for (int j=0; j < numCols; j++) {
                    System.out.print("  " + data[i][j]);
                }
                System.out.println();
            }
            System.out.println("--------------------------");
        }
	 }*/

}

