package de.tuhh.wb.tools.gui;

import javax.swing.JButton;
import javax.swing.event.AncestorEvent;
import javax.swing.event.AncestorListener;

import de.tuhh.wb.tools.event.GuiAction;
import de.tuhh.wb.tools.event.GuiElement;

public class GuiButton extends JButton implements GuiElement,AncestorListener
{
    private GuiAction myAction;

    public GuiButton(GuiAction action)
    {
	super();
	this.myAction=action;
	/*String label=I18n.get(action.getActionCommand(),"GUI_"+action.getActionCommand());
	Icon icon=Symbols.getIcon("ICON_"+action.getActionCommand());
	if(icon!=null)
	    {
		setIcon(icon);
		setToolTipText(label);
	    }
	else
	    setText(label);*/
	
	setActionCommand(action.getActionCommand());
	addActionListener(action);
	//	action.registerGuiElement(this);	
	addAncestorListener(this);
    }
    
    public void setEnabled(boolean enable)
    {
	super.setEnabled(enable);
    }
    
    // Called when the source or one of its ancestors is made visible either by setVisible(true) being called or by its being added to the component hierarchy.
    public void ancestorAdded(AncestorEvent event) 
    {
	myAction.registerGuiElement(this);
	setEnabled(myAction.hasListeners());
    }
    
    // Called when either the source or one of its ancestors is moved.
    public void ancestorMoved(AncestorEvent event)
    {
    }
    
    // Called when the source or one of its ancestors is made invisible either by setVisible(false) being called or by its being remove from the component hierarchy.
    public void ancestorRemoved(AncestorEvent event)
    {
	myAction.unregisterGuiElement(this);
    }
}
