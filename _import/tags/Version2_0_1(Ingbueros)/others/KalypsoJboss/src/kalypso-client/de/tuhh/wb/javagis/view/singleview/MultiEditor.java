

package de.tuhh.wb.javagis.view.singleview;



import java.awt.Color;
import java.awt.Component;
import java.awt.Insets;
import java.util.EventObject;

import javax.swing.DefaultCellEditor;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.event.CellEditorListener;
import javax.swing.table.TableCellEditor;

import de.tuhh.wb.javagis.data.GisElement;
import de.tuhh.wb.javagis.view.tableview.BCEChooser;
import de.tuhh.wb.javagis.view.tableview.DateChooser;
import de.tuhh.wb.javagis.view.tableview.DateEditor;
import de.tuhh.wb.javagis.view.tableview.TableCellEditorBCE;


public class MultiEditor implements TableCellEditor {

  private final static int        INT = 0;

  private final static int    BOOLEAN = 1;

  private final static int     STRING = 2;

  private final static int       DATE = 3;

  private final static int     BUTTON = 4;
	
  private final static int     FLOAT  = 5;

  private final static int     DOUBLE = 6;
	
  private final static int     BCE    = 7;
	
  private final static int NUM_EDITOR = 8;

  DefaultCellEditor[] cellEditors;

  JComboBox comboBox;

  int flg;

  DateEditor dateEditor;

  TableCellEditorBCE bceEditor;
	
  GisElement myGisElement = null;

  public MultiEditor(GisElement gisElement) {
		
	myGisElement = gisElement;


    cellEditors = new DefaultCellEditor[NUM_EDITOR];

    /**comboBox = new JComboBox();

       comboBox.addItem("true");

       comboBox.addItem("false");

       cellEditors[COMBO]   = new DefaultCellEditor(comboBox);*/

    JCheckBox checkBox   = new JCheckBox();

    JButton buttonDate       = new JButton();
	JButton buttonBce        = new JButton();
    //checkBox.setOpaque( true );

    checkBox.setHorizontalAlignment(JLabel.CENTER);

    cellEditors[BOOLEAN] = new DefaultCellEditor(checkBox);

    JTextField textField = new JTextField();

    cellEditors[STRING]  = new DefaultCellEditor(textField);

    cellEditors[INT]     = new DefaultCellEditor(textField);
	cellEditors[FLOAT]     = new DefaultCellEditor(textField);
	cellEditors[DOUBLE]     = new DefaultCellEditor(textField);
		
    buttonDate.setBackground(Color.white);
    buttonDate.setBorderPainted(false);
    buttonDate.setMargin(new Insets(0,0,0,0));
		
	buttonBce.setBackground(Color.white);
    buttonBce.setBorderPainted(false);
    buttonBce.setMargin(new Insets(0,0,0,0));


    dateEditor = new DateEditor(buttonDate);

    DateChooser.setUpDateEditor(dateEditor,buttonDate);

    cellEditors[DATE]    = dateEditor;
	
	bceEditor = new TableCellEditorBCE(buttonBce);
	
	BCEChooser.setUpBCEEditor(bceEditor,buttonBce);
	
	cellEditors[BCE] = bceEditor;
		

    flg = NUM_EDITOR;      // nobody

  }



    public Component getTableCellEditorComponent(JTable table, Object value,

						 boolean isSelected, int row, int column) {



	Class propClass = myGisElement.getSimplePropertyClass(row-1);
	System.out.println("PropClass: "+propClass.toString());

	if (propClass.toString().equals("class java.lang.Integer")){//value instanceof Integer){

	    flg = INT;

	    return cellEditors[INT].getTableCellEditorComponent(table,value,isSelected,row,column);

	} else if (propClass.toString().equals("class java.lang.Boolean")) {                    // Boolean

	    flg = BOOLEAN;

	    return cellEditors[BOOLEAN].getTableCellEditorComponent(

								    table, value, isSelected, row, column);

	} else if (propClass.toString().equals("class java.lang.String")) {                     // String

	    flg = STRING;

	    return cellEditors[STRING].getTableCellEditorComponent(

								   table, value, isSelected, row, column);

	}else if (propClass.toString().equals("class java.util.Date")) {			 //Date

	    flg = DATE;

		return cellEditors[DATE].getTableCellEditorComponent(table,value,isSelected,row,column);
	
	}else if (propClass.toString().equals("class de.tuhh.wb.javagis.data.TSLink")) {			 		//TSLink

	    flg = BCE;

	    return cellEditors[BCE].getTableCellEditorComponent(table,value,isSelected,row,column);
		
	} else if (propClass.toString().equals("class java.lang.Float")) {                    // Float

	    flg = FLOAT;

	    return cellEditors[FLOAT].getTableCellEditorComponent(

								    table, value, isSelected, row, column);
	
	} else if (propClass.toString().equals("class java.lang.Double")) {                    // Double

	    flg = DOUBLE;

	    return cellEditors[DOUBLE].getTableCellEditorComponent(

								    table, value, isSelected, row, column);

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
				
				case FLOAT:
				return Float.valueOf((String)value);
				
				case DOUBLE:
				return Double.valueOf((String)value);

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



