/*
 * Created on 15.07.2004
 *
 */
package org.kalypso.ui.editor.styleeditor.panels;

import javax.swing.event.EventListenerList;

import org.eclipse.swt.*;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.*;
import org.eclipse.swt.widgets.*;

/**
 * @author Administrator
 *
 */
public class ControlRulePanel {
	
	private Composite parent = null;
	private Composite composite = null;
	 
	private EventListenerList listenerList = new EventListenerList();
	private PanelEvent panelEvent = null;
	
	public final static int ADD_RULE = 0;
	public final static int REM_RULE = 1;
	public final static int FOR_RULE = 2;
	public final static int BAK_RULE = 3;
	private String labelText = null;
	private int currentAction = -1;
	private int canDelete = -1;
	
	
	public ControlRulePanel(Composite parent, String labelText, int size){
		this.parent = parent; 
		this.canDelete = size;	
		this.labelText = labelText;
		composite = new Composite(parent, SWT.NULL);			
		FormLayout compositeLayout = new FormLayout();
		GridData compositeData = new GridData();
		compositeData.widthHint = 240;
		composite.setLayoutData(compositeData);
		composite.setLayout(compositeLayout);	
		compositeLayout.marginWidth = 0;
		compositeLayout.marginHeight = 0;
		compositeLayout.spacing = 0;
		composite.layout();			
		init();
	}
	
	public void addPanelListener(PanelListener pl) {
		listenerList.add(PanelListener.class, pl);
	}

	
	private void init()
	{	
		Label label = new Label(composite, SWT.NULL);
		FormData labelData = new FormData();
		labelData.height = 15;
		labelData.width = 40;
		labelData.top =  new FormAttachment(100, 1000, 0);
		labelData.left =  new FormAttachment(10, 1000, 0);
		label.setLayoutData(labelData);
		label.setText(labelText);
		
		Button addRuleButton = new Button(composite,SWT.PUSH);			
		FormData addRuleButtonData = new FormData();
		addRuleButtonData.height = 18;
		addRuleButtonData.width = 18;
		addRuleButtonData.left =  new FormAttachment(400, 1000, 0);		
		addRuleButtonData.top =  new FormAttachment(100, 1000, 0);
		addRuleButton.setLayoutData(addRuleButtonData);		
		addRuleButton.setText("+");	
		addRuleButton.setToolTipText("Add rule");
		addRuleButton.addSelectionListener(new SelectionListener() {
			public void widgetSelected(SelectionEvent e) 
			{
				currentAction = ADD_RULE;
				fire();							
			}
			public void widgetDefaultSelected(SelectionEvent e) {
				widgetSelected(e);
			}
		});					
	
		Button removeRuleButton = new Button(composite,SWT.PUSH);
		if(canDelete == 0)
			removeRuleButton.setEnabled(false);
		FormData removeRuleButtonData = new FormData();
		removeRuleButtonData.height = 18;
		removeRuleButtonData.width = 18;
		removeRuleButtonData.left =  new FormAttachment(500, 1000, 0);		
		removeRuleButtonData.top =  new FormAttachment(100, 1000, 0);
		removeRuleButton.setLayoutData(removeRuleButtonData);		
		removeRuleButton.setText("-");
		removeRuleButton.setToolTipText("Remove rule");
		removeRuleButton.addSelectionListener(new SelectionListener() {
			public void widgetSelected(SelectionEvent e) 
			{
				currentAction = REM_RULE;
				fire();							
			}
			public void widgetDefaultSelected(SelectionEvent e) {
				widgetSelected(e);
			}
		});							
	
		Button moveBackwardRuleButton = new Button(composite,SWT.PUSH);
		if(canDelete <= 1)
			moveBackwardRuleButton.setEnabled(false);
		FormData moveBackwardRuleButtonData = new FormData();
		moveBackwardRuleButtonData.height = 18;
		moveBackwardRuleButtonData.width = 18;
		moveBackwardRuleButtonData.left =  new FormAttachment(750, 1000, 0);		
		moveBackwardRuleButtonData.top =  new FormAttachment(100, 1000, 0);
		moveBackwardRuleButton.setLayoutData(moveBackwardRuleButtonData);		
		moveBackwardRuleButton.setText("<-");
		moveBackwardRuleButton.setToolTipText("Move Backward");
		moveBackwardRuleButton.addSelectionListener(new SelectionListener() {
			public void widgetSelected(SelectionEvent e) 
			{
				currentAction = BAK_RULE;
				fire();							
			}
			public void widgetDefaultSelected(SelectionEvent e) {
				widgetSelected(e);
			}
		});	
		
		Button moveForwardRuleButton = new Button(composite,SWT.PUSH);
		if(canDelete <= 1)
			moveForwardRuleButton.setEnabled(false);		
		FormData moveForwardRuleButtonData = new FormData();
		moveForwardRuleButtonData.height = 18;
		moveForwardRuleButtonData.width = 18;
		moveForwardRuleButtonData.left =  new FormAttachment(850, 1000, 0);		
		moveForwardRuleButtonData.top =  new FormAttachment(100, 1000, 0);
		moveForwardRuleButton.setLayoutData(moveForwardRuleButtonData);		
		moveForwardRuleButton.setText("->");
		moveForwardRuleButton.setToolTipText("Move Forward");
		moveForwardRuleButton.addSelectionListener(new SelectionListener() {
			public void widgetSelected(SelectionEvent e) 
			{
				currentAction = FOR_RULE;
				fire();							
			}
			public void widgetDefaultSelected(SelectionEvent e) {
				widgetSelected(e);
			}
		});								
	}	
	
	public int getAction(){
		return currentAction;
	}
	
    protected void fire() {
        Object[] listeners = listenerList.getListenerList();
        for (int i = listeners.length - 2; i >= 0; i -= 2) {
        	if (listeners[i] == PanelListener.class) {
        		PanelEvent event = new PanelEvent(this);                
                ((PanelListener)listeners[i+1]).valueChanged(event);
        	}
        }
    }
}