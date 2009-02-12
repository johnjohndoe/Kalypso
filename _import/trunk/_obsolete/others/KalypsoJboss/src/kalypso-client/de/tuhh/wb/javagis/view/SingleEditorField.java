package de.tuhh.wb.javagis.view;

import javax.swing.JTable;

import de.tuhh.wb.javagis.view.tableview.GisTableFilter;

public class SingleEditorField extends JTable
{
    SingleEditorModel myModel;
    public SingleEditorField(GisTableFilter filter,String name,Class fieldClass)
    {
	super(new SingleEditorModel(filter,name,fieldClass,null));
    }
    
    public void reload(GisTableFilter filter,String name,Class fieldClass)
    {
	setModel(myModel=new SingleEditorModel(filter,name,fieldClass,null));
    }
}
