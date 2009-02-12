package de.tuhh.wb.javagis.simpleclient;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import de.tuhh.wb.javagis.tools.I18n;

public class ModelVersionDialog extends JPanel
{
    JLabel text;
    JTextField field;
   
    public ModelVersionDialog()
    {
	super();
	text=new JLabel(I18n.get("KF_ModelVersion"));
	field=new JTextField("0",8);
	add(text);
	add(field);
	doLayout();
    }

    public Integer getVersionNumber() throws Exception
    {
	return new Integer(field.getText());
    }


}
