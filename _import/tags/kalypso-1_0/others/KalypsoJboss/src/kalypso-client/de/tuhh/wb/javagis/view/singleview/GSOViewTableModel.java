

package de.tuhh.wb.javagis.view.singleview;



import javax.swing.table.AbstractTableModel;

import de.tuhh.wb.javagis.data.GisElement;

import de.tuhh.wb.javagis.data.GisElementClass;



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

		return myGisElement.getSimplePropertySize()+1;

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

	if(col==0)

	    {

		if(row==0)

		    return "ID";

		else

		    return myGisElement.getSimplePropertyName(row-1);

	    }

	else

	    {

		if(row==0)

		    return myGisElement.getId();

		else

		    return myGisElement.getSimplePropertyValue(row-1);

	    }

    }

    

    public void setValueAt(Object value,int row,int col)

    {

	if(col==0)

	    return;

	else

	    {

		myGisElement.setSimplePropertyValue(row-1,value);

	    }

    }

    

    public boolean isCellEditable(int row,int col)

    {

	if(col==0)
	    return false;

	else if(row==0)
	    return false;

	GisElementClass myGisElementClass = myGisElement.getGisElementClass();
	if(myGisElementClass.getVersion().isEditable()){
		return true;
	}else{
		return false;
	}

    }

	

    public boolean isBCEButton(int col)

    {

	GisElementClass myGisElementClass = myGisElement.getGisElementClass();

	if("bce_db".equals(myGisElementClass.getSimplePropertyFormat(col-1)))

	    return true;

	return false;

    }
	
	


}



