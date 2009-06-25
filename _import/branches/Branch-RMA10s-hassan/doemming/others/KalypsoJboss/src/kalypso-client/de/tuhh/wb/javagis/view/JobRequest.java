package de.tuhh.wb.javagis.view;

import java.awt.*;
import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;


public class JobRequest extends JFrame
{
    public JobRequest(String label,String actionCommand,ActionListener actionListener)
    {
	super(label);
	getContentPane().setLayout(new FlowLayout());
	
	JTextField textField = new JTextField ("eingabe",30);
	textField.setActionCommand(actionCommand);
	textField.addActionListener(actionListener);
	getContentPane().add(textField);
	setSize(350,100);
	show();
    }
}
