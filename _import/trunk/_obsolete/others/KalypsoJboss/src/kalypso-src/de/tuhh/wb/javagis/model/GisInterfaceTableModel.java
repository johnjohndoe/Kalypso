package de.tuhh.wb.javagis.model;

import java.util.Vector;

public interface GisInterfaceTableModel
{
    public int getColumnCount();
    public int getRowCount();
    public String getColumnName(int col);
    public String getDescription(int col);
    public Class getColumnClass(int col);
    public boolean isCellEditable(int row,int col);
    public Object getValueAt(int row,int col);
    public void setValueAt(Object value,int row,int col);
    public String getDescription();
    public String getName();
    
    // am Anfang der Liste neues Object erzeugen
    public void createNewObject();
    
    // am Ende der Liste neues Object erzeugen
    public void appendNewObject();

    public void insertNewObjectAt(int position);

    public void removeObject(int position);

    // von "startIndex" an soundsoviele Objecte loeschen
    public void removeObjects(int startIndex, int number);

    public boolean hasVectorSets();
    public Vector getVectorSetTableModels(int row);
}
