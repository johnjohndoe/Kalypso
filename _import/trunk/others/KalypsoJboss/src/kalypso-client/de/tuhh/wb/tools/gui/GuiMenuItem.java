package de.tuhh.wb.tools.gui;

import javax.swing.JMenuItem;
import javax.swing.event.AncestorEvent;
import javax.swing.event.AncestorListener;

import de.tuhh.wb.tools.event.GuiAction;
import de.tuhh.wb.tools.event.GuiElement;

public class GuiMenuItem extends JMenuItem implements GuiElement, AncestorListener
{
    private GuiAction myAction;

    public GuiMenuItem(GuiAction action)
    {
	//super(I18n.get(action.getActionCommand(),"GUI_"+action.getActionCommand()));
	super();
	this.myAction=action;
	setActionCommand(action.getActionCommand());
	addActionListener(action);
	addAncestorListener(this);
    }
    
    public void setEnabled(boolean enable)
    {
	super.setEnabled(enable);
    }
    
    // Called when the source or one of its ancestors is made visible either by setVisible(true) being called or by its being added to the component hierarchy.
    public void ancestorAdded(AncestorEvent event) 
    {
	//Debug.println("guimenu: ancestor added");
	//	myAction.registerGuiElement(this);
	//	setEnabled(myAction.hasListeners());		
    }
    
    // Called when either the source or one of its ancestors is moved.
    public void ancestorMoved(AncestorEvent event)
    {
	//Debug.println("guimenu: ancestor moved");
	myAction.registerGuiElement(this);
	setEnabled(myAction.hasListeners());		
	// ToDo: unregisterGuiElement somewhere
    }


    
    // Called when the source or one of its ancestors is made invisible either by setVisible(false) being called or by its being remove from the component hierarchy.
    public void ancestorRemoved(AncestorEvent event)
    {
	//Debug.println("guimenu: ancestor removed");
	myAction.unregisterGuiElement(this);
    }    
}
