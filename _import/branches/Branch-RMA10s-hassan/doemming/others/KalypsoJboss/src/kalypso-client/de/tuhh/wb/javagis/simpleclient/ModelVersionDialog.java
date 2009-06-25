package de.tuhh.wb.javagis.simpleclient;
import java.awt.event.TextListener;
import java.awt.event.TextEvent;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import javax.swing.JPanel;
import javax.swing.JLabel;
import javax.swing.JTextField;
import javax.swing.event.DocumentListener;
import javax.swing.event.DocumentEvent;

public class ModelVersionDialog extends JPanel
{
    JLabel text;
    JTextField field;
   
    public ModelVersionDialog()
    {
	super();
	text=new JLabel("ModelVersion:");
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
