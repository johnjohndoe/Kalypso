package de.tuhh.wb.javagis.simpleclient;
import javax.swing.table.AbstractTableModel;
import java.util.Vector;
import de.tuhh.wb.javagis.tools.I18n;
public class CatchmentDialogModel extends AbstractTableModel 
{
    Vector rows;
    public CatchmentDialogModel()
    {
	super();
	rows=new Vector();
    }
    
    public Class getColumnClass(int columnIndex) 
    {
	return Integer.class;
    }

    public int getColumnCount() 
    {
	return 1;
    }

    public String getColumnName(int columnIndex) 
    {
	return "<html>catchments<br>No.</html>";
    }

    public int getRowCount()
    {
	return rows.size();
    }

    public Object getValueAt(int rowIndex, int columnIndex) 
    {
	return rows.elementAt(rowIndex);
    }

    public boolean isCellEditable(int rowIndex, int columnIndex) 
    {
	return true;
    }

    public void setValueAt(Object value, int rowIndex, int columnIndex)
    {
	if(value==null)
	{
	    rows.removeElementAt(rowIndex);	
	}
	else
	    rows.setElementAt(value,rowIndex);
	update();
    }
    
    public void addRow()
    {
	rows.add(new Integer(0));
	update();
    }

    public void addRow(Integer value)
    {
	rows.add(value);
	update();
    }
    

    public void update()
    {
	java.util.Collections.sort(rows);
	fireTableDataChanged();	
    }
}
