
package de.tuhh.wb.javagis.view.singleview;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.table.*;
import javax.swing.event.*;
import java.util.EventObject;

import java.util.Calendar;
import java.util.TimeZone;
import java.util.Locale;

import de.tuhh.wb.javagis.jcalendar.src.com.toedter.calendar.JCalendar;
import de.tuhh.wb.javagis.jcalendar.src.com.toedter.calendar.JMonthChooser;
import de.tuhh.wb.javagis.jcalendar.src.com.toedter.calendar.JCalendarField;

import de.tuhh.wb.javagis.view.tableview.DateEditor;
import de.tuhh.wb.javagis.view.tableview.DateChooser;

public class MultiEditor implements TableCellEditor {
  private final static int        INT = 0;
  private final static int    BOOLEAN = 1;
  private final static int     STRING = 2;
  private final static int       DATE = 3;
  private final static int     BUTTON = 4;
  private final static int NUM_EDITOR = 5;
  DefaultCellEditor[] cellEditors;
  JComboBox comboBox;
  int flg;
  DateEditor dateEditor;

  public MultiEditor() {
    cellEditors = new DefaultCellEditor[NUM_EDITOR];
    /**comboBox = new JComboBox();
       comboBox.addItem("true");
       comboBox.addItem("false");
       cellEditors[COMBO]   = new DefaultCellEditor(comboBox);*/
    JCheckBox checkBox   = new JCheckBox();
    JButton button       = new JButton();
    //checkBox.setOpaque( true );
    checkBox.setHorizontalAlignment(JLabel.CENTER);
    cellEditors[BOOLEAN] = new DefaultCellEditor(checkBox);
    JTextField textField = new JTextField();
    cellEditors[STRING]  = new DefaultCellEditor(textField);
    cellEditors[INT]     = new DefaultCellEditor(textField);
    button.setBackground(Color.white);
    button.setBorderPainted(false);
    button.setMargin(new Insets(0,0,0,0));

    dateEditor = new DateEditor(button);
    DateChooser.setUpDateEditor(dateEditor,button);
    cellEditors[DATE]    = dateEditor;
    flg = NUM_EDITOR;      // nobody
  }

    public Component getTableCellEditorComponent(JTable table, Object value,
						 boolean isSelected, int row, int column) {

	
	if (value instanceof Integer){
	    flg = INT;
	    return cellEditors[INT].getTableCellEditorComponent(table,value,isSelected,row,column);
	    /**if (value instanceof ComboString) {                       // ComboString
	       flg = COMBO;
	       String str = (value == null) ? "" : value.toString();
	       return cellEditors[COMBO].getTableCellEditorComponent(
	       table, str,   isSelected, row, column);*/
	} else if (value instanceof Boolean) {                    // Boolean
	    flg = BOOLEAN;
	    return cellEditors[BOOLEAN].getTableCellEditorComponent(
								    table, value, isSelected, row, column);
	} else if (value instanceof String) {                     // String
	    flg = STRING;
	    return cellEditors[STRING].getTableCellEditorComponent(
								   table, value, isSelected, row, column);
	}else if (value instanceof java.util.Date) {			 //Date
	    flg = DATE;
	    return cellEditors[DATE].getTableCellEditorComponent(table,value,isSelected,row,column);
	}
	return null;
    }

    public Object getCellEditorValue() {
	Object value=cellEditors[flg].getCellEditorValue();

	if(value instanceof String)
	    {
		try
		    {
			switch(flg)
			    {
			    case STRING:
				return value;
			    case INT:
				return Integer.decode((String)value);
			    default:
				return null;
			    }
		    }
		catch(Exception e)
		    {
			return null;
		    }
	    }
	
	return value;
	/*	switch (flg) {
//case   COMBO:
//	       String str = (String)comboBox.getSelectedItem();
//	       return new ComboString(str);
	case	DATE:
	    cellEditors[DATE].getCellEditorValue();
	case BOOLEAN:
	case STRING:
	case INT:
	    System.out.println("Tabelleneintrag: "+cellEditors[flg].getCellEditorValue());
	    return cellEditors[flg].getCellEditorValue();

      default:         return null;
  }
*/
  
  }

  public Component getComponent() {
    return cellEditors[flg].getComponent();
  }
  public boolean stopCellEditing() {
    return cellEditors[flg].stopCellEditing();
  }
  public void cancelCellEditing() {
    cellEditors[flg].cancelCellEditing();
  }
  public boolean isCellEditable(EventObject anEvent) {
    //return cellEditors[flg].isCellEditable(anEvent);
	  return true;
  }
  public boolean shouldSelectCell(EventObject anEvent) {
    return cellEditors[flg].shouldSelectCell(anEvent);
  }
  public void addCellEditorListener(CellEditorListener l) {
    cellEditors[flg].addCellEditorListener(l);
  }
  public void removeCellEditorListener(CellEditorListener l) {
    cellEditors[flg].removeCellEditorListener(l);
  }
  public void setClickCountToStart(int n) {
    cellEditors[flg].setClickCountToStart(n);
  }
  public int getClickCountToStart() {
    return cellEditors[flg].getClickCountToStart();
  }
}

