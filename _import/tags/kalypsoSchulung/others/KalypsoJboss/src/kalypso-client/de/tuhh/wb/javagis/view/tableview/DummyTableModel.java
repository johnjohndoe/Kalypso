package de.tuhh.wb.javagis.view.tableview;
import javax.swing.table.AbstractTableModel;
import java.util.Hashtable;
import java.util.Vector;
import java.util.Enumeration;
import javax.swing.table.AbstractTableModel;
import de.tuhh.wb.javagis.model.GisInterfaceTableModel;
import de.tuhh.wb.javagis.tools.I18n;

public class DummyTableModel extends AbstractTableModel implements GisInterfaceTableModel
{
    private GisInterfaceTableModel model;
	String language = I18n.getLanguage();

    public DummyTableModel(GisInterfaceTableModel model)
    {
	this.model=model;
    }

    public int getColumnCount()
    {
	return model.getColumnCount();
    }
    public int getRowCount()
    {
	return model.getRowCount();
    }
    public String getColumnName(int col)
    {
		if(language.equals("eng"))
		   return model.getColumnName(col);
		else
			return I18n.get("DTMVectorSetColumn_"+model.getColumnName(col));
    }
    public String getDescription(int col)
    {
		if(language.equals("eng"))
		 	return model.getDescription(col);
		else
			return I18n.get("DTMVectorSetColumnDescription_"+model.getDescription(col));
    }
    public Class getColumnClass(int col)
    {
	return model.getColumnClass(col);
    }
    public boolean isCellEditable(int row,int col)
    {
	return model.isCellEditable(row, col);
    }
    public Object getValueAt(int row,int col)
    {
	return model.getValueAt(row, col);
    }
    public void setValueAt(Object value,int row,int col)
    {
	 model.setValueAt( value, row, col);
    }
    public String getDescription()
    {
		if(language.equals("eng"))
		 	return model.getDescription();
		else
			return I18n.get("DTMVectorSetDescription_"+model.getDescription());
    }
    public String getName()
    {
		if(language.equals("eng"))
		   return model.getName();
		else
			return I18n.get("DTMVectorSetName_"+model.getName());
    }
    
    // am Anfang der Liste neues Object erzeugen
    public void createNewObject()
    {
	model.createNewObject();
    }
    
    // am Ende der Liste neues Object erzeugen
    public void appendNewObject()
    {
	model.appendNewObject();
    }

    public void insertNewObjectAt(int position)
    {
	model.insertNewObjectAt( position);
    }

    public void removeObject(int position)
    {
	model.removeObject( position);
    }

    // von "startIndex" an soundsoviele Objecte loeschen
    public void removeObjects(int startIndex, int number)
    {
	model.removeObjects( startIndex,  number);
    }

    public boolean hasVectorSets()
    {
	return model.hasVectorSets();
    }

    public Vector getVectorSetTableModels(int row)
    {
	return model.getVectorSetTableModels(row);
    }

}
