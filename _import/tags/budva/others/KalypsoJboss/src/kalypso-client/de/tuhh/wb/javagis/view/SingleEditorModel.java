package de.tuhh.wb.javagis.view;

import javax.swing.table.AbstractTableModel;
import de.tuhh.wb.javagis.view.tableview.GisTableFilter;
public class SingleEditorModel extends AbstractTableModel
{
    private Class myFieldClass;
    private String myName;
    private Object myValue;
    private GisTableFilter myFilter;
    public SingleEditorModel(GisTableFilter filter,String name,Class fieldClass,Object value)
    {
	super();
	this.myName=name;
	this.myFieldClass=fieldClass;
	this.myFilter=filter;
	this.myValue=value;
    }
    
    public Class getColumnClass(int columnIndex) 
    {
	return myFieldClass;
    }

    public int getColumnCount() 
    {
	return 1;
    }

    public String getColumnName(int columnIndex) 
    {
	return myName;
    }

    public int getRowCount() 
    {
	return 1;
    }

    public boolean isCellEditable(int rowIndex, int columnIndex) 
    {
	return true;
    }
    
    public Object getValueAt(int rowIndex, int columnIndex) 
    {
	return myValue;
    }

    public void setValueAt(Object value, int rowIndex, int columnIndex) 
    {
	this.myValue=value;
	myFilter.start(value);
    }

}
