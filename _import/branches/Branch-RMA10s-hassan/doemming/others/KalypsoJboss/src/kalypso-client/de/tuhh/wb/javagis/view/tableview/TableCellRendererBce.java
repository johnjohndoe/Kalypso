package de.tuhh.wb.javagis.view.tableview;

import javax.swing.table.AbstractTableModel;

//import de.tuhh.wb.javagis.model.ElementSession;
import java.util.Hashtable;
import java.util.Vector;
import java.util.Enumeration;
import de.tuhh.wb.javagis.view.GisView;
import javax.swing.event.TableModelEvent;
import java.awt.Component;
import de.tuhh.wb.javagis.Main;
import de.tuhh.wb.javagis.data.*;
import de.tuhh.wb.javagis.data.event.ElementClassListener;
import javax.swing.JButton;
import javax.swing.JTable;
import de.tuhh.wb.javagis.model.GisInterfaceTableModel;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.text.DateFormat;
import java.util.Date;
import java.util.Comparator;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableCellEditor;
import java.util.EventObject;
import javax.swing.event.CellEditorListener;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import timeserieSelection.CTSStruct;
import timeserieSelection.BceBrowser;


public class TableCellRendererBce implements TableCellRenderer,TableCellEditor ,ActionListener
{
    private GisTableModel myModel;
    private JTable myJTable;
    private Hashtable buttons;
    private int myCol; //from Model
   
    public TableCellRendererBce(JTable jTable,GisTableModel model,int col)
    {
	this.myModel=model;
	this.myJTable=jTable;
	this.buttons=new Hashtable();
	this.myCol=col;
    }

    private JButton getButton(int row)
    {
	Object value=myModel.getValueAt(row,myCol);
	Object id=myModel.getId(row);
	if(!buttons.containsKey(id))
	    {
		JButton newButton=new JButton();
		newButton.addActionListener(this);
		newButton.setActionCommand("bceSelect");
		buttons.put(id,newButton);
	    }
	JButton button=(JButton)buttons.get(id);
	if(value==null)
	    {
		button.setText("choose...");
		button.setToolTipText("");
	    }
	else
	    {
		String text=value.toString();
		int trim1=text.lastIndexOf("/");
		int trim2=text.indexOf(",");
		String name=text.substring(trim1+1,trim2);
		button.setText(name);
		button.setToolTipText(value.toString());
	    }
	return button;
    }
    
    public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int col) 
    {
	return getButton(row);
    }

    public Component getTableCellEditorComponent(JTable table,
						 Object value,
						 boolean isSelected,
						 int row,
						 int col)
    {
	System.out.println("getEditor");
	return getButton(row);
    }
    
    public void addCellEditorListener(CellEditorListener l) 
    {
	System.out.println("addCellEditorListener");
    }

    public void cancelCellEditing() 
    {
	System.out.println("cancelCellEditor");
    }
    //   Tells the editor to cancel editing and not accept any partially edited value.

    public Object getCellEditorValue() 
    {
	System.out.println("getCellEditorValue");
	return "value";
    }
    //    Returns the value contained in the editor.

    public boolean isCellEditable(EventObject anEvent) 
    {
	return true;
    }
    //          Asks the editor if it can start editing using anEvent.
    
    public void removeCellEditorListener(CellEditorListener l) 
    {
	System.out.println("removeCellEditorListener");
    }
    //          Removes a listener from the list that's notified

    public boolean shouldSelectCell(EventObject anEvent) 
    {
	System.out.println("shouldSelectCell");
	return true;
    }
    //	Returns true if the editing cell should be selected, false otherwise.

    public boolean stopCellEditing() 
    {
	System.out.println("stopCellEditing");
	return true;
    }
    public void actionPerformed(ActionEvent e)
    {
	String command=e.getActionCommand();
	if("bceSelect".equals(command))
	    {
		BceBrowser browser=BceBrowser.getInstance();
		if(browser.isIcon())
		    browser.show();
		else
		    {
			int row=myJTable.getEditingRow();
			//			System.out.println(",row "+row);
			
			//			String selectedNode=Main.getBceBrowserTable();
			String selectedNode=browser.getSelectedNode();
			//		    myModel.setValueAt(selectedNode.m_name+","+selectedNode.m_tableName,row,col);
			myModel.setValueAt(selectedNode,row,myCol);
			myJTable.tableChanged(new TableModelEvent(myModel, row, row, myCol));
			getButton(row);		    
		    }
	    }
    }
}
