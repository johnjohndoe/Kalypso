package de.tuhh.wb.javagis.simpleclient;

import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import javax.swing.JPanel;
import javax.swing.JLabel;
import javax.swing.JTextField;
import javax.swing.event.DocumentListener;
import javax.swing.event.DocumentEvent;

public class FileDialog extends JPanel implements ActionListener
{
    File model;
    JButton button;
    
    public FileDialog(File templateDir)
    {
	super();
	model=new File(templateDir,"default.xml");
	button=new JButton("");
	add(button);
	updateButton();
	doLayout();
    }

    private void updateButton()
    {
	button.setText("Model: "+model.getName());
	button.setToolTipText("Model: "+model.toString());	
    }

    public File getModel()
    {
	return model;
    }

    public 
}
