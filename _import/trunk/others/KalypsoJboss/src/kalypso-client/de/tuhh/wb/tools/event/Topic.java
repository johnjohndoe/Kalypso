package de.tuhh.wb.tools.event;

import java.awt.event.ActionListener;
import java.util.Hashtable;

import javax.swing.JButton;
import javax.swing.JMenuItem;

public class Topic
{
    private String name;
    private Hashtable actionHash=new Hashtable();

    public Topic(String name)
    {
	this.name=name;
    }
    
    private GuiAction getGuiAction(String command)
    {
	if(!actionHash.containsKey(command))
	    actionHash.put(command,new GuiAction(name+"."+command));
	return (GuiAction)actionHash.get(command);
    }
    
    public void add(String actionCommand,ActionListener listener)
    {
	//System.out.println("Topic:"+name);
	getGuiAction(actionCommand).addActionListener(listener);
    }

    public void remove(String actionCommand,ActionListener listener)
    {
		System.out.println("Topic:"+name);
		getGuiAction(actionCommand).removeActionListener(listener);
    }
    
    public JButton getJButton(String actionCommand)
    {
	return getGuiAction(actionCommand).getJButton();
    }

    public JMenuItem getJMenuItem(String actionCommand)
    {
	return getGuiAction(actionCommand).getJMenuItem();
    }
}
