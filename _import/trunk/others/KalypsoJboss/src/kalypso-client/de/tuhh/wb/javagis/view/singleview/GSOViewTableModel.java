
package de.tuhh.wb.javagis.view.singleview;

import javax.swing.table.AbstractTableModel;
import de.tuhh.wb.javagis.data.GisElement;

public class GSOViewTableModel extends AbstractTableModel
{
	private GisElement myGisElement;
	
	public GSOViewTableModel (GisElement gisElement){
	myGisElement = gisElement;
	}
	public int getColumnCount()
    {
		return 2;
	 //return myGisElementClass.getSimplePropertySize()+1;
    }

    public int getRowCount()
    {
	//return myIdList.size();
		return myGisElement.getSimplePropertySize();
    }
	    public String getColumnName(int col)
    {
		if (col==0)
		return "SimplePropertyName";
		if (col==1)
		return "SimplePropertyValue";
		else
		return "";
    }
	    public Class getColumnClass(int col)
    {
	    return String.class;
    }
	public Object getValueAt(int row,int col)
	{
		if (col==0)
		return myGisElement.getSimplePropertyName(row);
		if (col==1)
		return myGisElement.getSimplePropertyValue(row);
		else
		return "";
	}
	public void setValueAt(Object value,int row,int col)
    {
	if(col==0)
	    return;
	else
	    {
	    myGisElement.setSimplePropertyValue(col-1,value);
	    }
	}
    
	
	
	    public boolean isCellEditable(int row,int col)
    {
	if(col==0)
	    return false;
	else
	    return true;
    }
}

