package de.tuhh.wb.javagis.simpleclient;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import de.tuhh.wb.javagis.tools.I18n;
import de.tuhh.wb.javagis.xml.GisTransferObject;

public class RootNodeDialog extends JPanel
{
    JLabel text;
    JTextField field;

    public RootNodeDialog()
    {
	super();
	text=new JLabel("    "+I18n.get("KF_RootNode"));
	field=new JTextField("7170",8);
	add(text);
	add(field);
	doLayout();
    }

    public Integer getNode()
    {
	return new Integer(field.getText());
    }

    public void storeToGto(GisTransferObject gto) throws Exception
    {
	gto.addSimpleProperty("rootNode",field.getText());
    }

    public void loadFromGto(GisTransferObject gto)
    {
	String text=gto.getSimpleProperty("rootNode");
	if(text!=null)
	    field.setText(text);
    }   
}
