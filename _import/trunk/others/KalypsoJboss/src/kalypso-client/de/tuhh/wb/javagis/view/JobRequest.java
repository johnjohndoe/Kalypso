package de.tuhh.wb.javagis.view;

import java.awt.FlowLayout;
import java.awt.event.ActionListener;

import javax.swing.JFrame;
import javax.swing.JTextField;


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
